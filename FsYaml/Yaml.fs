module FsYaml.Yaml

let load<'a> = Representation.parse >> Native.construct<'a> TypeDefinitions.defaultDefinitions
let load2<'a> customDefinitions = Representation.parse >> Native.construct<'a> (Seq.append customDefinitions TypeDefinitions.defaultDefinitions)

let tryLoad<'a> yamlStr =
  try
    Some (load<'a> yamlStr)
  with
    _ -> None

let tryLoad2<'a> customDefinitions yamlStr =
  try
    Some (load2<'a> customDefinitions yamlStr)
  with
    _ -> None