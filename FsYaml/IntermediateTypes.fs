namespace FsYaml

module IntermediateTypes =
  type ScalarType =
    | NonQuoted of string
    | Quoted of string

  and SequenceType = YamlNode list

  and MappingType = Map<ScalarType, YamlNode>

  and YamlNode =
    | Scalar of ScalarType
    | Sequence of SequenceType
    | Mapping of MappingType
