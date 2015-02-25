module TypesTest

open Persimmon
open UseTestNameByReflection
open Assertions

module MappingTest =
  open FsYaml.RepresentationTypes
  open System.Collections.Generic

  let ``findで見つからない場合は例外が発生する`` = test {
    let m = Map.ofList [ (Scalar (Plain "1", None), Scalar (Plain "2", None)) ]
    let! e = trap { it (Mapping.find "3" m) }
    do! e.GetType() |> should equal typeof<KeyNotFoundException>
  }
