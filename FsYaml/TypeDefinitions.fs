module FsYaml.TypeDefinitions

open Microsoft.FSharp.Reflection
open System
open FsYaml.FsYamlException
open FsYaml.Utility
open FsYaml.IntermediateTypes
open FsYaml.NativeTypes

let scalarDefinition t f = {
  Accept = ((=) t)
  Construct = fun construct' t yaml ->
    match yaml with
    | Scalar (s, _) -> Scalar.value s |> f |> box
    | _ -> loadingError yaml "%s must be scalar." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let nullableScalarDefinition t f = {
  Accept = ((=) t)
  Construct = fun construct' t yaml ->
    match yaml with
    | Scalar (s, _) -> Scalar.value s |> f |> box
    | Null _ -> null
    | _ -> loadingError yaml "%s must be scalar." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let isGenericType expected (x: Type) = x.IsGenericType && x.GetGenericTypeDefinition() = expected

let intDef = scalarDefinition typeof<int> int
let int64Def = scalarDefinition typeof<int64> int64
let floatDef = scalarDefinition typeof<float> (String.toLower >> function
  | ".inf" | "+.inf" -> Double.PositiveInfinity
  | "-.inf" -> Double.NegativeInfinity
  | ".nan" -> Double.NaN
  | otherwise -> float otherwise)
let stringDef = nullableScalarDefinition typeof<string> id
let boolDef = scalarDefinition typeof<bool> (String.toLower >> function
  | "y" | "yes" | "on" -> true
  | "n" | "no"  | "off" -> false
  | otherwise -> Boolean.Parse(otherwise))
let decimalDef = scalarDefinition typeof<decimal> decimal
let datetimeDef = scalarDefinition typeof<DateTime> (fun x -> DateTime.Parse(x))
let timespanDef = scalarDefinition typeof<TimeSpan> (fun x -> TimeSpan.Parse(x))

let recordDef = {
  Accept = (fun t -> FSharpType.IsRecord(t))
  Construct = fun construct' t yaml ->
    match yaml with
    | Mapping (mapping, _) ->
      let values =
        FSharpType.GetRecordFields(t)
        |> Array.map (fun p ->
          match Mapping.tryFind p.Name mapping with
          | Some valueObj -> construct' p.PropertyType valueObj
          | None -> loadingError yaml "%s.%s key was not found." (Type.print t) (p.Name))
      FSharpValue.MakeRecord(t, values)
    | _ -> loadingError yaml "%s must be mapping." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
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
      | None -> loadingError yaml "%s must have %d elements." (Type.print t) (Array.length elementTypes)
    | _ -> loadingError yaml "%s must be sequence." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let listDef = {
  Accept = (isGenericType typedefof<list<_>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetGenericArguments().[0]
      let elements = sequence |> List.map (construct' elementType)
      ObjectSeq.toList elementType elements
    | _ -> loadingError yaml "%s should be sequence." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let mapDef = {
  Accept = (isGenericType typedefof<Map<_, _>>)
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
      ObjectSeq.toMap keyType valueType values
    | _ -> loadingError yaml "%s should be mapping." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let arrayDef = {
  Accept = (fun t -> t.IsArray)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetElementType()
      let values = Seq.map (construct' elementType) sequence
      ObjectSeq.toArray elementType values
    | _ -> loadingError yaml "%s should be sequence." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let seqDef = {
  Accept = (isGenericType typedefof<seq<_>>)
  Construct = fun construct' t yaml ->
    match yaml with
    | Sequence (sequence, _) ->
      let elementType = t.GetGenericArguments().[0]
      let xs = Seq.map (construct' elementType) sequence
      ObjectSeq.cast elementType xs
    | _ -> loadingError yaml "%s should be sequence." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

module Union =
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
      | None -> loadingError parentYamlForExceptionMessage "%s.%s case must have %d elements." union.DeclaringType.Name union.Name fieldTypes.Length
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

let unionDef = {
  Accept = fun t -> FSharpType.IsUnion(t)
  Construct = fun construct' t yaml ->
    match FSharpType.GetUnionCases(t) |> Seq.tryPick (Union.tryConstruct construct' yaml) with
    | Some x -> x
    | None -> loadingError yaml "Any %s cases was not found." (Type.print t)
  Represent = fun _ -> raise (NotImplementedException())
}

let optionDef = {
  Accept = fun t -> FSharpType.IsUnion(t) && isGenericType typedefof<Option<_>> t
  Construct = fun construct' t yaml ->
    let noneCase, someCase = let xs = FSharpType.GetUnionCases(t) in (xs.[0], xs.[1])
    match yaml with
    | Null _ -> (Union.makeUnion noneCase [])
    | _ ->
      try
        let parameterType = t.GetGenericArguments().[0]
        let value = construct' parameterType yaml
        Union.makeUnion someCase [ value ]
      with _ ->  unionDef.Construct construct' t yaml 
  Represent = fun _ -> raise (NotImplementedException())
}

let defaultDefinitions = [ intDef; int64Def; floatDef; stringDef; boolDef; decimalDef; datetimeDef; timespanDef; recordDef; tupleDef; listDef; mapDef; arrayDef; seqDef; optionDef; unionDef ]