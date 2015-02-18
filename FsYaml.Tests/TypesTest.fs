module TypesTest

open NUnit.Framework
open FsUnit

[<TestFixture>]
module MappingTest =
  open FsYaml.RepresentationTypes
  open System.Collections.Generic

  [<Test>]
  [<ExpectedException(typeof<KeyNotFoundException>)>]
  let ``findで見つからない場合は例外が発生する``() =
    let m = Map.ofList [ (Scalar (Plain "1", None), Scalar (Plain "2", None)) ]
    Mapping.find "3" m |> ignore