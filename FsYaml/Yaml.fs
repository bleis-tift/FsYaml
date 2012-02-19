module Yaml

open FParsec

open YamlParser
open YamlDumper

let load<'a> yamlStr: 'a =
  let parser = ws >>. pbody typeof<'a> .>> ws .>> followedBy eof
  match yamlStr |> FParsec.CharParsers.runParserOnString parser (Context.make pspace_block) "" with
  | Success(res, _, _) -> unbox res
  | Failure(msg, err, state) -> failwithf "msg: %s\nerr: %A\nstate: %A" msg err state

let tryLoad yamlStr =
  try
    Some (load yamlStr)
  with
    _ -> None

let dump x = dump 0 x