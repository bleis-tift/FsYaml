module internal FsYaml.Native

open FsYaml.Utility
open FsYaml.NativeTypes

let constructZero definitions t =
  match definitions |> Seq.tryFind (fun d -> d.Accept t) with
  | Some d -> d.Zero t
  | None -> None

let rec construct' definitions t yaml =
  match definitions |> Seq.tryFind (fun d -> d.Accept t) with
  | Some d ->
    let recC = construct' definitions
    let zero = constructZero definitions
    d.Construct recC zero t yaml
  | None -> raise (FsYamlException.WithYaml(yaml, Messages.typeDefinitionNotFound, Type.print t))

let construct<'a> definitions yaml = construct' definitions typeof<'a> yaml :?> 'a

let rec represent' definitions t value =
  match definitions |> Seq.tryFind (fun d -> d.Accept t) with
  | Some d ->
    let recR = represent' definitions
    d.Represent recR t value
  | None -> raise (FsYamlException.Create(Messages.typeDefinitionNotFound, Type.print t))

let represent<'a> definitions (value: 'a) = represent' definitions typeof<'a> value