module internal FsYaml.TypeDefinitions

open Microsoft.FSharp.Reflection
open System
open FsYaml.Utility
open FsYaml.RepresentationTypes
open FsYaml.NativeTypes
open FsYaml.CustomTypeDefinition

let intDef = { Accept = (=)typeof<int>; Construct = constructFromScalar int; Represent = representAsPlain string }
let int64Def = { Accept = (=)typeof<int64>; Construct = constructFromScalar int64; Represent = representAsPlain string }
let floatDef = {
  Accept = (=)typeof<float>
  Construct = constructFromScalar (String.toLower >> function
    | ".inf" | "+.inf" -> Double.PositiveInfinity
    | "-.inf" -> Double.NegativeInfinity
    | ".nan" -> Double.NaN
    | otherwise -> float otherwise)
  Represent = fun represent t obj ->
    let n = unbox<float> obj
    let text =
      if Double.IsNaN(n) then ".NaN"
      elif Double.IsPositiveInfinity(n) then "+.inf"
      elif Double.IsNegativeInfinity(n) then "-.inf"
      else string n
    Scalar (Plain text, None)
}
let stringDef = { Accept = (=)typeof<string>; Construct = MaybeNull.constructFromScalar id; Represent = MaybeNull.representAsNonPlain string }
let boolDef = {
  Accept = (=)typeof<bool>
  Construct = constructFromScalar (String.toLower >> function
    | "y" | "yes" | "on" -> true
    | "n" | "no"  | "off" -> false
    | otherwise -> Boolean.Parse(otherwise))
  Represent = fun represent t obj ->
    let text = unbox<bool> obj |> string |> String.toLower 
    Scalar (Plain text, None)
}
let decimalDef = { Accept = (=)typeof<decimal>; Construct = constructFromScalar decimal; Represent = representAsPlain string }
let datetimeDef = {
  Accept = (=)typeof<DateTime>
  Construct = constructFromScalar (fun x -> DateTime.Parse(x))
  Represent = representAsNonPlain (fun x ->
    let d = unbox<DateTime> x
    d.ToString("yyyy-MM-dd HH:mm:ss.fff"))
}
let timespanDef = {
  Accept = (=)typeof<TimeSpan>
  Construct = constructFromScalar (fun x -> TimeSpan.Parse(x))
  Represent = representAsNonPlain (fun x ->
    let t = unbox<TimeSpan> x
    t.ToString(@"hh\:mm\:ss\.fff")
  )
}

module RecordConstructor =
  open System.Reflection

  let tryGetDefaultValueFromStaticField (field: PropertyInfo) =
    let t = field.DeclaringType
    let name = sprintf "Default%s" field.Name
    let defaultValueProperty = t.GetProperty(name, BindingFlags.Public ||| BindingFlags.Static)
    if defaultValueProperty = null then
      None
    else
      let value = defaultValueProperty.GetValue(null)
      let expectedType = field.PropertyType
      let actualType = defaultValueProperty.PropertyType
      if expectedType.IsAssignableFrom(actualType) then
        Some value
      else
        raise (FsYamlException.Create(Resources.getString "invalidDefaultValueType", PropertyInfo.print defaultValueProperty, Type.print expectedType, Type.print actualType))

  let tryGetDefaultValueFromOption (field: PropertyInfo) =
    let t = field.PropertyType
    if t |> isGenericTypeDef typedefof<option<_>> then
      Some null // None is null
    else
      None

  let tryGetDefaultValue (field: PropertyInfo) =
    match tryGetDefaultValueFromStaticField field with
    | Some _ as x -> x
    | None -> tryGetDefaultValueFromOption field

  let tryFindFieldValue construct' yaml mapping (field: PropertyInfo) =
    match Mapping.tryFind field.Name mapping with
    | Some valueObj -> Some (construct' field.PropertyType valueObj)
    | None -> tryGetDefaultValue field

  let construct construct' t yaml =
    match yaml with
    | Mapping (mapping, _) ->
      let values =
        FSharpType.GetRecordFields(t)
        |> Array.map (fun field ->
          match tryFindFieldValue construct' yaml mapping field with
          | Some valueObj -> valueObj
          | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "recordFieldNotFound", PropertyInfo.print field)))
      FSharpValue.MakeRecord(t, values)
    | otherwise -> raise (mustBeMapping t otherwise)

module RecordRepresenter =
  let represent represent t obj =
    let values =
      FSharpType.GetRecordFields(t)
      |> Seq.map (fun field ->
        let name = Scalar (Plain field.Name, None)
        let value = represent field.PropertyType (field.GetValue(obj))
        (name, value)
      )
      |> Map.ofSeq
    Mapping (values, None)

let recordDef = {
  Accept = (fun t -> FSharpType.IsRecord(t))
  Construct = RecordConstructor.construct
  Represent = RecordRepresenter.represent
}

let tupleDef = {
  Accept = (fun t -> FSharpType.IsTuple(t))
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementTypes = FSharpType.GetTupleElements(t)
      match Seq.tryZip elementTypes sequence with
      | Some xs ->
        let tupleValues = xs |> Seq.map (fun (elementType, node) -> construct' elementType node) |> Seq.toArray
        FSharpValue.MakeTuple(tupleValues, t)
      | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "tupleElementNumber", Type.print t, Array.length elementTypes))
    | otherwise -> raise (mustBeSequence t otherwise)
  Represent = fun represent t obj ->
    let values =
      Seq.zip (FSharpType.GetTupleElements(t)) (FSharpValue.GetTupleFields(obj))
      |> Seq.map (fun (elemType, elemValue) -> represent elemType elemValue)
      |> Seq.toList
    Sequence (values, None)
}

let listDef = {
  Accept = (isGenericTypeDef typedefof<list<_>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetGenericArguments().[0]
      let elements = sequence |> List.map (construct' elementType)
      ObjectElementSeq.toList elementType elements
    | otherwise -> raise (mustBeSequence t otherwise)
  Represent = representSeqAsSequence
}

let setDef = {
  Accept = (isGenericTypeDef typedefof<Set<_>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetGenericArguments().[0]
      let elements = sequence |> List.map (construct' elementType)
      ObjectElementSeq.toSet elementType elements
    | otherwise -> raise (mustBeSequence t otherwise)
  Represent = representSeqAsSequence
}

let mapDef = {
  Accept = (isGenericTypeDef typedefof<Map<_, _>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Mapping (mapping, _) ->
      let keyType, valueType = let ts = t.GetGenericArguments() in (ts.[0], ts.[1])
      let values =
        mapping
        |> Seq.map (fun (KeyValue(keyYaml, valueYaml)) ->
          let key = construct' keyType keyYaml
          let value = construct' valueType valueYaml
          (key, value)
        )
      ObjectElementSeq.toMap keyType valueType values
    | otherwise -> raise (mustBeMapping t otherwise)
  Represent = fun represent t obj ->
    let keyType, valueType = RuntimeMap.elementTypes t
    let values =
      RuntimeMap.toSeq t obj
      |> Seq.map (fun (key, value) ->
        let key =
          match represent keyType key with
          | Scalar _ as s -> s
          | otherwise -> raise (FsYamlException.Create(Resources.getString "mapKeyMustBeScalar", Type.print t, YamlObject.nodeTypeName otherwise))
        let value = represent valueType value
        (key, value)
      )
      |> Map.ofSeq
    Mapping (values, None)
}

let arrayDef = {
  Accept = (fun t -> t.IsArray)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetElementType()
      let values = Seq.map (construct' elementType) sequence
      ObjectElementSeq.toArray elementType values
    | otherwise -> raise (mustBeSequence t otherwise)
  Represent = representSeqAsSequence
}

let seqDef = {
  Accept = (isGenericTypeDef typedefof<seq<_>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetGenericArguments().[0]
      let xs = Seq.map (construct' elementType) sequence
      ObjectElementSeq.cast elementType xs
    | otherwise -> raise (mustBeSequence t otherwise)
  Represent = representSeqAsSequence
}

module UnionConstructor =
  let makeUnion (union: UnionCaseInfo) values = FSharpValue.MakeUnion(union, Seq.toArray values)

  let noFieldCase yaml (union: UnionCaseInfo) =
    match yaml with
    | Scalar (scalar, _) ->
      let name = Scalar.value scalar
      if name = union.Name then
        Some (makeUnion union [])
      else
        None
    | _ -> None

  let tryNamedFieldCase construct' (union: UnionCaseInfo) (mapping: Map<YamlObject, YamlObject>) =
    let fields = union.GetFields()
    let yamls = fields |> Array.choose (fun field -> Mapping.tryFind field.Name mapping)
        
    Seq.tryZip fields yamls
    |> Option.map (fun xs ->
      xs
      |> Seq.map (fun (field, yaml) -> construct' field.PropertyType yaml)
      |> makeUnion union
    )

  let caseWithFields construct' (union: UnionCaseInfo) yamls (parentYamlForExceptionMessage: YamlObject) =
    let fieldTypes = union.GetFields()
    let fieldValues =
      match Seq.tryZip fieldTypes yamls with
      | Some xs -> xs |> Seq.map (fun (t, yaml) -> construct' t.PropertyType yaml) |> Seq.toArray
      | None -> raise (FsYamlException.WithYaml(parentYamlForExceptionMessage, Resources.getString "unionCaseElementNumber", (Union.printCase union), fieldTypes.Length))
    makeUnion union fieldValues

  let oneFieldCase construct' yaml (union: UnionCaseInfo) =
    match yaml with
    | Mapping (mapping, _) ->
      Mapping.tryFind union.Name mapping
      |> Option.bind (fun value ->
        let maybeNamedField =
          match value with
          | Mapping (mapping, _) -> tryNamedFieldCase construct' union mapping
          | _ -> None
        match maybeNamedField with
        | Some named -> Some named
        | None -> Some (caseWithFields construct' union [ value ] yaml)
      )
    | _ -> None

  let manyFieldsCase construct' yaml (union: UnionCaseInfo) =
    match yaml with
    | Mapping (mapping, _) ->
      Mapping.tryFind union.Name mapping
      |> Option.bind (function
        | Sequence (sequence, _) -> Some (caseWithFields construct' union sequence yaml)
        | Mapping (mapping, _) -> tryNamedFieldCase construct' union mapping
        | _ -> None
      )
    | _ -> None

  let tryConstruct construct' yaml (union: UnionCaseInfo) =
    let fields = union.GetFields()
    match fields.Length with
    | 0 -> noFieldCase yaml union
    | 1 -> oneFieldCase construct' yaml union
    | _ -> manyFieldsCase construct' yaml union

module UnionRepresenter =
  let caseName (union: UnionCaseInfo) = Scalar (Plain union.Name, None)

  let oneField represent (union: UnionCaseInfo) (value: obj) =
    let fields = union.GetFields()
    let valueType = fields.[0].PropertyType
    let value = represent valueType value
    Mapping (Map.ofList [caseName union, value ], None)

  let manyFields represent (union: UnionCaseInfo) (values: obj[]) =
    let fields = union.GetFields()
    let fieldValues =
      let xs =
        Seq.zip fields values
        |> Seq.map (fun (field, value) -> represent field.PropertyType value)
        |> Seq.toList
      Sequence (xs, None)
    Mapping (Map.ofList [ caseName union, fieldValues ], None)

  let isNamedFieldCase (union: UnionCaseInfo) =
    let fields = union.GetFields()
    match fields.Length with
    | 0 -> false
    | 1 -> fields.[0].Name <> "Item"
    | _ -> fields |> Array.mapi (fun i field -> (i + 1, field)) |> Array.forall (fun (n, field) -> field.Name <> sprintf "Item%d" n)

  let namedField represent (union: UnionCaseInfo) (values: obj[]) =
    let fields = union.GetFields()
    let values =
      Seq.zip fields values
      |> Seq.map (fun (field, value) ->
        let name = Scalar (Plain field.Name, None)
        let value = represent field.PropertyType value
        (name, value)
      )
      |> Map.ofSeq
    let name = caseName union
    let fieldMapping = Mapping (values, None)
    Mapping (Map.ofList [ name, fieldMapping ], None)

  let represent (represent: RecursiveRepresenter) (t: Type) (obj: obj) =
    let union, values = FSharpValue.GetUnionFields(obj, t)
    if isNamedFieldCase union then
      namedField represent union values
    else
      match values.Length with
      | 0 -> caseName union
      | 1 -> oneField represent union values.[0]
      | _ -> manyFields represent union values 

let unionDef = {
  Accept = fun t -> FSharpType.IsUnion(t)
  Construct = fun construct' t yaml ->
    match FSharpType.GetUnionCases(t) |> Seq.tryPick (UnionConstructor.tryConstruct construct' yaml) with
    | Some x -> x
    | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "unionCaseNotFound", Type.print t))
  Represent = UnionRepresenter.represent
}

let optionDef = {
  Accept = fun t -> FSharpType.IsUnion(t) && isGenericTypeDef typedefof<Option<_>> t
  Construct = fun construct' t yaml ->
    let noneCase, someCase = let xs = FSharpType.GetUnionCases(t) in (xs.[0], xs.[1])
    match yaml with
    | Null _ -> (UnionConstructor.makeUnion noneCase [])
    | _ ->
      try
        let parameterType = t.GetGenericArguments().[0]
        let value = construct' parameterType yaml
        UnionConstructor.makeUnion someCase [ value ]
      with _ ->  unionDef.Construct construct' t yaml
  Represent = fun represent t obj ->
    match obj with
    | null -> Null None
    | _ ->
      let caseInfo, values = FSharpValue.GetUnionFields(obj, t)
      let valueType = caseInfo.GetFields().[0].PropertyType
      represent valueType values.[0]
}

let defaultDefinitions = [ intDef; int64Def; floatDef; stringDef; boolDef; decimalDef; datetimeDef; timespanDef; recordDef; tupleDef; listDef; setDef; mapDef; arrayDef; seqDef; optionDef; unionDef ]