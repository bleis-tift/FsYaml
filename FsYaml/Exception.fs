namespace FsYaml

open System

type FsYamlException(msg, ex: exn) =
  inherit Exception(msg, ex)

  new (msg) = FsYamlException(msg, null)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FsYamlException =
  open FsYaml.IntermediateTypes

  let tryAppendPosition position msg =
    match position with
    | Some p -> sprintf "%s (Line=%d, Column=%d)" msg p.Line p.Column
    | None -> msg

  let loadingError (yaml: YamlObject) format =
    Printf.kprintf (fun msg -> raise (FsYamlException(tryAppendPosition (YamlObject.position yaml) msg))) format

  let loadingError2 position format =
    Printf.kprintf (fun msg -> raise (FsYamlException(tryAppendPosition position msg))) format

  let loadingError3 ex position format =
    Printf.kprintf (fun msg -> raise (FsYamlException(tryAppendPosition position msg, ex))) format

  let dumpingError format =
    Printf.kprintf (fun msg -> raise (FsYamlException(msg))) format