module FsYaml.TypeDefinitions

open Microsoft.FSharp.Reflection
open System
open FsYaml.Utility
open FsYaml.RepresentationTypes
open FsYaml.NativeTypes
open FsYaml.CustomTypeDefinition

module internal Detail =
  let intDef = {
    Accept = (=)typeof<int>
    FuzzyConstruct = fuzzyConstructFromScalar PrefersPlain int
    Represent = representAsPlain string
  }
  let int64Def = {
    Accept = (=)typeof<int64>
    FuzzyConstruct = fuzzyConstructFromScalar PrefersPlain int64
    Represent = representAsPlain string
  }
  module FloatConstructor =
    let ofString s =
      match s |> String.toLower with
      | ".inf" | "+.inf" -> Double.PositiveInfinity
      | "-.inf" -> Double.NegativeInfinity
      | ".nan" -> Double.NaN
      | otherwise -> float otherwise

  let floatDef = {
    Accept = (=)typeof<float>
    FuzzyConstruct = fuzzyConstructFromScalar PrefersPlain FloatConstructor.ofString
    Represent = fun represent t obj ->
      let n = unbox<float> obj
      let text =
        if Double.IsNaN(n) then ".NaN"
        elif Double.IsPositiveInfinity(n) then "+.inf"
        elif Double.IsNegativeInfinity(n) then "-.inf"
        else string n
      Scalar (Plain text, None)
  }
  let stringDef = {
    Accept = (=)typeof<string>
    FuzzyConstruct = MaybeNull.fuzzyConstructFromScalar PrefersNonPlain id
    Represent = MaybeNull.representAsNonPlain string
  }
  module BoolConstructor =
    let ofString s =
      match s |> String.toLower with
      | "y" | "yes" | "on" -> true
      | "n" | "no"  | "off" -> false
      | otherwise -> Boolean.Parse(otherwise)
  let boolDef = {
    Accept = (=)typeof<bool>
    FuzzyConstruct = fuzzyConstructFromScalar PrefersPlain BoolConstructor.ofString
    Represent = fun represent t obj ->
      let text = unbox<bool> obj |> string |> String.toLower 
      Scalar (Plain text, None)
  }

  let decimalDef = {
    Accept = (=)typeof<decimal>
    FuzzyConstruct = fuzzyConstructFromScalar PrefersPlain decimal
    Represent = representAsPlain string
  }
  let datetimeDef = {
    Accept = (=)typeof<DateTime>
    FuzzyConstruct = fuzzyConstructorFromConstructor (constructFromScalar (fun x -> DateTime.Parse(x)))
    Represent = representAsNonPlain (fun x ->
      let d = unbox<DateTime> x
      d.ToString("yyyy-MM-dd HH:mm:ss.fff"))
  }
  let timespanDef = {
    Accept = (=)typeof<TimeSpan>
    FuzzyConstruct = fuzzyConstructorFromConstructor (constructFromScalar (fun x -> TimeSpan.Parse(x)))
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

    let tryFindFieldValue fuzzyConstruct' yaml mapping (field: PropertyInfo) =
      match Mapping.tryFind field.Name mapping with
      | Some valueObj -> Some (fuzzyConstruct' field.PropertyType valueObj)
      | None -> tryGetDefaultValue field |> Option.map Fuzzy.result

    let fuzzyConstruct fuzzyConstruct' t yaml =
      match yaml with
      | Mapping (mapping, _) ->
        let values =
          FSharpType.GetRecordFields(t)
          |> Array.map (fun field ->
            match tryFindFieldValue fuzzyConstruct' yaml mapping field with
            | Some valueObj -> valueObj
            | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "recordFieldNotFound", PropertyInfo.print field)))
          |> Fuzzy.flatten
        values |> Fuzzy.map (fun values -> FSharpValue.MakeRecord(t, Array.ofList values))
      | otherwise -> raise (mustBeMapping t otherwise)

  module RecordRepresenter =
    open System.Reflection
    open RecordConstructor

    /// Returns the default value as an YamlObject if the field can be omitted in YAML
    let tryRepresentDefaultValue (represent: RecursiveRepresenter) (field: PropertyInfo): option<YamlObject> =
      field |> tryGetDefaultValue |> Option.map (represent field.PropertyType)

    let represent omitsDefaultFields represent t obj =
      let values =
        FSharpType.GetRecordFields(t)
        |> Seq.choose (fun field ->
          let name = Scalar (Plain field.Name, None)
          let value = represent field.PropertyType (field.GetValue(obj))
          if omitsDefaultFields && Some value = tryRepresentDefaultValue represent field
          then None  // Omit if it's the default value of the field
          else Some (name, value)
        )
        |> Map.ofSeq
      Mapping (values, None)

  let recordDef = {
    Accept = (fun t -> FSharpType.IsRecord(t))
    FuzzyConstruct = RecordConstructor.fuzzyConstruct
    Represent = RecordRepresenter.represent ((* omitDefaultFields: *) false)
  }

  let tupleDef = {
    Accept = (fun t -> FSharpType.IsTuple(t))
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Sequence (sequence, _) ->
        let elementTypes = FSharpType.GetTupleElements(t)
        match Seq.tryZip elementTypes sequence with
        | Some xs ->
          let tupleValues = xs |> Seq.map (fun (elementType, node) -> fuzzyConstruct' elementType node) |> Fuzzy.flatten
          tupleValues |> Fuzzy.map (fun values -> FSharpValue.MakeTuple(Array.ofList values, t))
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
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Sequence (sequence, _) ->
        let elementType = t.GetGenericArguments().[0]
        let elements = sequence |> List.map (fuzzyConstruct' elementType) |> Fuzzy.flatten
        elements |> Fuzzy.map (fun elements -> ObjectElementSeq.toList elementType elements)
      | otherwise -> raise (mustBeSequence t otherwise)
    Represent = representSeqAsSequence
  }

  let setDef = {
    Accept = (isGenericTypeDef typedefof<Set<_>>)
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Sequence (sequence, _) ->
        let elementType = t.GetGenericArguments().[0]
        let elements = sequence |> List.map (fuzzyConstruct' elementType) |> Fuzzy.flatten
        elements |> Fuzzy.map (fun elements -> ObjectElementSeq.toSet elementType elements)
      | otherwise -> raise (mustBeSequence t otherwise)
    Represent = representSeqAsSequence
  }

  let mapDef = {
    Accept = (isGenericTypeDef typedefof<Map<_, _>>)
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Mapping (mapping, _) ->
        let keyType, valueType = let ts = t.GetGenericArguments() in (ts.[0], ts.[1])
        let values =
          mapping
          |> Seq.map (fun (KeyValue(keyYaml, valueYaml)) ->
            fuzzyConstruct' keyType keyYaml |> Fuzzy.bind (fun key ->
            fuzzyConstruct' valueType valueYaml |> Fuzzy.map (fun value ->
              (key, value)
          )))
          |> Fuzzy.flatten
        values |> Fuzzy.map (fun values -> ObjectElementSeq.toMap keyType valueType values)
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
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Sequence (sequence, _) ->
        let elementType = t.GetElementType()
        let values = Seq.map (fuzzyConstruct' elementType) sequence |> Fuzzy.flatten
        values |> Fuzzy.map (fun values -> ObjectElementSeq.toArray elementType values)
      | otherwise -> raise (mustBeSequence t otherwise)
    Represent = representSeqAsSequence
  }

  let seqDef = {
    Accept = (isGenericTypeDef typedefof<seq<_>>)
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      match yaml with
      | Sequence (sequence, _) ->
        let elementType = t.GetGenericArguments().[0]
        let xs = Seq.map (fuzzyConstruct' elementType) sequence |> Fuzzy.flatten
        xs |> Fuzzy.map (fun xs -> ObjectElementSeq.cast elementType xs)
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
          Some (makeUnion union [] |> Fuzzy.result)
        else
          None
      | _ -> None

    let tryNamedFieldCase fuzzyConstruct' (union: UnionCaseInfo) (mapping: Map<YamlObject, YamlObject>) =
      let fields = union.GetFields()
      let yamls = fields |> Array.choose (fun field -> Mapping.tryFind field.Name mapping)
        
      Seq.tryZip fields yamls
      |> Option.map (fun xs ->
        xs
        |> Seq.map (fun (field, yaml) -> fuzzyConstruct' field.PropertyType yaml)
        |> Fuzzy.flatten
        |> Fuzzy.map (makeUnion union)
      )

    let caseWithFields fuzzyConstruct' (union: UnionCaseInfo) yamls (parentYamlForExceptionMessage: YamlObject) =
      let fieldTypes = union.GetFields()
      let fieldValues =
        match Seq.tryZip fieldTypes yamls with
        | Some xs -> xs |> Seq.map (fun (t, yaml) -> fuzzyConstruct' t.PropertyType yaml) |> Fuzzy.flatten |> Fuzzy.map Seq.toArray
        | None -> raise (FsYamlException.WithYaml(parentYamlForExceptionMessage, Resources.getString "unionCaseElementNumber", (Union.printCase union), fieldTypes.Length))
      fieldValues |> Fuzzy.map (fun fieldValues -> makeUnion union fieldValues)

    let oneFieldCase fuzzyConstruct' yaml (union: UnionCaseInfo) =
      match yaml with
      | Mapping (mapping, _) ->
        Mapping.tryFind union.Name mapping
        |> Option.bind (fun value ->
          let maybeNamedField =
            match value with
            | Mapping (mapping, _) -> tryNamedFieldCase fuzzyConstruct' union mapping
            | _ -> None
          match maybeNamedField with
          | Some named -> Some named
          | None -> Some (caseWithFields fuzzyConstruct' union [ value ] yaml)
        )
      | _ -> None

    let manyFieldsCase fuzzyConstruct' yaml (union: UnionCaseInfo) =
      match yaml with
      | Mapping (mapping, _) ->
        Mapping.tryFind union.Name mapping
        |> Option.bind (function
          | Sequence (sequence, _) -> Some (caseWithFields fuzzyConstruct' union sequence yaml)
          | Mapping (mapping, _) -> tryNamedFieldCase fuzzyConstruct' union mapping
          | _ -> None
        )
      | _ -> None

    let tryFuzzyConstruct fuzzyConstruct' yaml (union: UnionCaseInfo) =
      let fields = union.GetFields()
      match fields.Length with
      | 0 -> noFieldCase yaml union
      | 1 -> oneFieldCase fuzzyConstruct' yaml union
      | _ -> manyFieldsCase fuzzyConstruct' yaml union

    let tryFuzzyConstructWithInference fuzzyConstruct' _ yaml (union: UnionCaseInfo) =
      try
        let fields = union.GetFields()
        match fields.Length with
        | 0 ->
          match yaml with
          | Null _ -> makeUnion union [] |> Fuzzy.result |> Some
          | _ -> None
        | 1 ->
          fuzzyConstruct' fields.[0].PropertyType yaml
          |> Fuzzy.map (fun x -> makeUnion union [x]) |> Some
        | _ ->
          match yaml with
          | Sequence (sequence, _) ->
            Seq.tryZip fields sequence |> Option.map (fun xs ->
              xs |> Seq.map (fun (field, yaml) -> fuzzyConstruct' field.PropertyType yaml) |> Fuzzy.flatten
              |> Fuzzy.map (makeUnion union))
          | _ -> None
      with | _ -> None

    let fuzzyConstruct fuzzyConstruct' t yaml =
      let infers = Attribute.tryGetCustomAttribute<Attributes.InferUnionCaseAttribute> t |> Option.isSome
      if infers then
        let candidates =
           FSharpType.GetUnionCases(t)
           |> Seq.collect (fun union -> [tryFuzzyConstructWithInference fuzzyConstruct' t yaml union; tryFuzzyConstruct fuzzyConstruct' yaml union])
           |> Seq.choose id
           |> Seq.maxListBy Fuzzy.probability
        match candidates with
        | [result] -> result
        | [] -> raise (FsYamlException.WithYaml(yaml, Resources.getString "unionCaseNotFound", Type.print t))
        | _ -> raise (FsYamlException.WithYaml(yaml, Resources.getString "unionCaseInferenceRace", Type.print t))
      else
        match FSharpType.GetUnionCases(t) |> Seq.tryPick (tryFuzzyConstruct fuzzyConstruct' yaml) with
        | Some x -> x
        | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "unionCaseNotFound", Type.print t))

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
    FuzzyConstruct = UnionConstructor.fuzzyConstruct
    Represent = UnionRepresenter.represent
  }

  let optionDef = {
    Accept = fun t -> FSharpType.IsUnion(t) && isGenericTypeDef typedefof<Option<_>> t
    FuzzyConstruct = fun fuzzyConstruct' t yaml ->
      let noneCase, someCase = let xs = FSharpType.GetUnionCases(t) in (xs.[0], xs.[1])
      match yaml with
      | Null _ -> (UnionConstructor.makeUnion noneCase [] |> Fuzzy.result)
      | _ ->
        try
          let parameterType = t.GetGenericArguments().[0]
          let value = fuzzyConstruct' parameterType yaml
          value |> Fuzzy.map (fun value -> UnionConstructor.makeUnion someCase [ value ])
        with _ ->  unionDef.FuzzyConstruct fuzzyConstruct' t yaml
    Represent = fun represent t obj ->
      match obj with
      | null -> Null None
      | _ ->
        let caseInfo, values = FSharpValue.GetUnionFields(obj, t)
        let valueType = caseInfo.GetFields().[0].PropertyType
        represent valueType values.[0]
  }

open Detail

/// <summary>
/// レコードの型定義で、dump においてデフォルト値が設定されているフィールドを出力しないバージョンです。
/// </summary>
let recordDefOmittingDefaultFields =
  { recordDef with Represent = RecordRepresenter.represent ((* omitDefaultFields: *) true) }
  |> typeDefinitionFromInternalTypeDefinition

let internal defaultInternalDefinitions = [ intDef; int64Def; floatDef; stringDef; boolDef; decimalDef; datetimeDef; timespanDef; recordDef; tupleDef; listDef; setDef; mapDef; arrayDef; seqDef; optionDef; unionDef ]
let internal defaultDefinitions = defaultInternalDefinitions |> List.map typeDefinitionFromInternalTypeDefinition
