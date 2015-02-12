module FsYaml.Representation

open YamlDotNet.Core
open YamlDotNet.RepresentationModel
open YamlDotNet.Core.Events
open System.IO
open FsYaml.Utility
open FsYaml.FsYamlException
open FsYaml.IntermediateTypes
open System.Collections.Generic

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

let rec intermediateToYamlDotNet (yaml: YamlObject) =
  match yaml with
  | Scalar (Plain value, _) ->
    let node = YamlScalarNode(value)
    node.Style <- ScalarStyle.Plain
    node :> YamlNode
  | Scalar (NonPlain value, _) ->
    let node = YamlScalarNode(value)
    node.Style <- ScalarStyle.DoubleQuoted
    node :> YamlNode
  | Sequence (sequence, _) ->
    let children = sequence |> List.map intermediateToYamlDotNet
    let node = YamlSequenceNode(children)
    node.Style <- SequenceStyle.Block
    node :> YamlNode
  | Mapping (mapping, _) ->
    let node = YamlMappingNode()
    node.Style <- MappingStyle.Block

    let children =
      mapping
      |> Seq.iter (fun (KeyValue(k, v)) ->
        let key = intermediateToYamlDotNet k
        let value = intermediateToYamlDotNet v
        node.Children.Add(key, value)
      )
    
    node :> YamlNode
  | Null _ ->
    let node = YamlScalarNode("null")
    node.Style <- ScalarStyle.Plain
    node :> YamlNode

let toYamlString (yaml: YamlNode) =
  let stream = YamlStream(YamlDocument(yaml))
  let writer = new StringWriter()
  stream.Save(writer)
  let str = writer.ToString()
  str.Substring(0, str.Length - 5) // remove "...\r\n"

let present = intermediateToYamlDotNet >> toYamlString