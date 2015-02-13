namespace FsYaml

open System
open FsYaml.IntermediateTypes

type FsYamlException(msg, ex: exn) =
  inherit Exception(msg, ex)

  new (msg) = FsYamlException(msg, null)

  static member Create(ex, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    FsYamlException(msg, ex)

  static member Create(format, [<ParamArray>] args: obj[]) = FsYamlException.Create(null, format, args)

  static member WithPosition(ex, position, format, [<ParamArray>] args: obj[]) =
    let msg = String.Format(format, args)
    let msg =
      match position with
      | Some p -> sprintf "%s (Line=%d, Column=%d)" msg p.Line p.Column
      | None -> msg
    FsYamlException(msg, ex)

  static member WithPosition(position, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(null, position, format, args)

  static member WithYaml(ex, yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithPosition(ex, YamlObject.position yaml, format, args)

  static member WithYaml(yaml, format, [<ParamArray>] args: obj[]) = FsYamlException.WithYaml(null, yaml, format, args)

open FSharp.Configuration

type private R = ResXProvider<file = "Resources.resx">
type internal Messages = R.Resources