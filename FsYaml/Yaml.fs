module FsYaml.Yaml

let load<'a> = Representation.parse >> Native.construct<'a> TypeDefinitions.defaultDefinitions
let loadWith<'a> customDefinitions = Representation.parse >> Native.construct<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

let tryLoad<'a> yamlStr =
  try
    Some (load<'a> yamlStr)
  with
    _ -> None

let tryLoadWith<'a> customDefinitions yamlStr =
  try
    Some (loadWith<'a> customDefinitions yamlStr)
  with
    _ -> None

let dump<'a> = Native.represent<'a> TypeDefinitions.defaultDefinitions >> Representation.present
let dumpWith<'a> customDefinitions = Native.represent<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions) >> Representation.present