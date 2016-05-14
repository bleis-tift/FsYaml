module internal FsYaml.Native

open FsYaml.Utility
open FsYaml.NativeTypes
open FsYaml.TypeDefinitions

let rec fuzzyConstruct' definitions t yaml =
  match definitions |> Seq.tryFind (fun d -> d.Accept t) with
  | Some d ->
    let recC = fuzzyConstruct' definitions
    try
      d.FuzzyConstruct recC t yaml
    with
      | :? FsYamlException -> reraise()
      | ex -> raise (FsYamlException.WithYaml(ex, yaml, Resources.getString "failedConstruct", Type.print t))
  | None -> raise (FsYamlException.WithYaml(yaml, Resources.getString "typeDefinitionNotFound", Type.print t))

let fuzzyConstruct<'a> definitions yaml = fuzzyConstruct' definitions typeof<'a> yaml |> Fuzzy.map (fun x -> x :?> 'a)

let rec represent' (definitions: seq<TypeDefinition>) t value =
  match definitions |> Seq.tryFind (fun d -> d.Accept t) with
  | Some d ->
    let recR = represent' definitions
    try
      d.Represent recR t value
    with
      | :? FsYamlException -> reraise()
      | ex -> raise (FsYamlException.Create(ex, Resources.getString "failedRepresent", Type.print t))
  | None -> raise (FsYamlException.Create(Resources.getString "typeDefinitionNotFound", Type.print t))

let represent<'a> definitions (value: 'a) = represent' definitions typeof<'a> value