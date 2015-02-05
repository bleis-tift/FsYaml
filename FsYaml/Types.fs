namespace FsYaml

module IntermediateTypes =
  type Scalar =
    | Plain of string
    | NonPlain of string
  with
    override x.ToString() = sprintf "%A" x

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Scalar =
    let value = function
      | Plain x -> x
      | NonPlain x -> x

  type Position = { Line: int; Column: int; }

  type YamlObject =
    | Scalar of Scalar * Position option
    | Sequence of YamlObject list * Position option
    | Mapping of Map<YamlObject, YamlObject> * Position option
    | Null of Position option
  with
    override x.ToString() = sprintf "%A" x

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module YamlObject =
    let position = function
      | Scalar (_, p) -> p
      | Sequence (_, p) -> p
      | Mapping (_, p) -> p
      | Null p -> p

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Mapping =
    let private pickF name = (fun k v ->
      match k with
      | Scalar (k, _) -> if Scalar.value k = name then Some v else None
      | _ -> None)
    let tryFind name (mapping: Map<YamlObject, YamlObject>) = Map.tryPick (pickF name) mapping
    let find name (mapping: Map<YamlObject, YamlObject>) = Map.pick (pickF name) mapping

module NativeTypes =
  open System
  open IntermediateTypes

  type Constructor = Type -> YamlObject -> obj
  type RecursiveConstructor = Constructor

  type Representer = Type -> obj -> YamlObject
  type RecursiveRepresenter = Representer

  type TypeDefinition = {
    Accept: Type -> bool
    Construct: RecursiveConstructor -> Constructor
    Represent: RecursiveRepresenter -> Representer
  }