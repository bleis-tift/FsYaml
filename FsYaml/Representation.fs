module FsYaml.Representation

open YamlDotNet.Core
open YamlDotNet.RepresentationModel
open System.IO
open FsYaml.Utility
open FsYaml.FsYamlException
open FsYaml.IntermediateTypes

let getPosition (mark: Mark) = Some { Line = mark.Line; Column = mark.Column }

let parseYaml str =
  try
    use input = new StringReader(str)
    let stream = YamlStream()
    stream.Load(input)

    (Seq.head stream).RootNode
  with
    | :? YamlException as ex ->
      let position = getPosition ex.Start
      loadingError3 ex position "%s" "Error in parsing yaml."
    | ex -> loadingError3 ex None "%s" "Error in parsing yaml."

let rec yamlDotNetToIntermediate (node: YamlNode) =
  let position = getPosition node.Start
  match node with
  | :? YamlScalarNode as scalarNode ->
    match scalarNode.Style with
    | ScalarStyle.SingleQuoted | ScalarStyle.DoubleQuoted | ScalarStyle.Literal | ScalarStyle.Folded -> Scalar (NonPlain scalarNode.Value, position)
    | ScalarStyle.Plain ->
      match String.toLower scalarNode.Value with
      | "" | "null" | "~" -> Null position
      | _ -> Scalar (Plain scalarNode.Value, position)
    | unsupported -> loadingError2 position "%A is not supported scalar type." unsupported
  | :? YamlSequenceNode as seqNode ->
    let children =
      seqNode.Children
      |> Seq.map yamlDotNetToIntermediate
      |> Seq.toList
    Sequence (children, position)
  | :? YamlMappingNode as mappingNode ->
    let mapping =
      mappingNode.Children
      |> Seq.map (fun (KeyValue(key, value)) ->
        let key =
          match yamlDotNetToIntermediate key with
          | Scalar _ as key -> key
          | _ -> loadingError2 position "The mapping key should be scalar."
        let value = yamlDotNetToIntermediate value
        (key, value)
      )
      |> Map.ofSeq
    Mapping(mapping, position)
  | unsupported -> loadingError2 position "%s is not supported node." (unsupported.GetType().Name)
    
let parse = parseYaml >> yamlDotNetToIntermediate