module DumpingTest

open Persimmon
open UseTestNameByReflection
open Assertions

open FsYaml
open FsYaml.RepresentationTypes

let represent<'a> value = Native.represent<'a> TypeDefinitions.defaultDefinitions value

let scalar x = Scalar (x, None)
let plain x = scalar (Plain x)
let nonPlain x = scalar (NonPlain x)
let sequence x = Sequence (x, None)
let mapping x = Mapping (Map.ofSeq x, None)
let null' = Null None

module DumpTest =
  open System

  let ``intを変換できる`` = test {
    let actual = represent 1
    do! actual |> should equal (plain "1")
  }

  let ``int64を変換できる`` = test {
    let actual = represent 20L
    do! actual |> should equal (plain "20")
  }

  let ``floatを変換できる`` =
    let body (value, expected) = test {
      let actual = represent value
      do! actual |> should equal (plain expected)
    }
    parameterize {
      case (Double.NaN, ".NaN")
      case (Double.PositiveInfinity, "+.inf")
      case (Double.NegativeInfinity, "-.inf")
      case (45.5, "45.5")
      run body
    }
    
  let ``stringを変換できる`` = test {
    let actual = represent "this is text"
    do! actual |> should equal (nonPlain "this is text")
  }

  let ``nullのstringを変換できる`` = test {
    let actual = represent (null: string)
    do! actual |> should equal null'
  }

  let ``boolを変換できる`` =
    let body (value, expected) = test {
      let actual = represent value
      do! actual |> should equal (plain expected)
    }
    parameterize {
      case (true, "true")
      case (false, "false")
      run body
    }
    
  let ``decimalを変換できる`` = test {
    let actual = represent 30.5m
    do! actual |> should equal (plain "30.5")
  }

  let ``DateTimeを変換できる`` = test {
    let actual = represent (DateTime.Parse("2015-02-12 11:22:33.111"))
    do! actual |> should equal (nonPlain "2015-02-12 11:22:33.111")
  }

  let ``TimeSpanを変換できる`` = test {
    let actual = represent (TimeSpan.Parse("11:22:33.444"))
    do! actual |> should equal (nonPlain "11:22:33.444")
  }

  type Record = { FieldA: int; FieldB: string }
  let ``recordを変換できる`` = test {
    let actual = represent { FieldA = 20; FieldB = "abc" }
    do! actual |> should equal (mapping [ (plain "FieldA", plain "20"); (plain "FieldB", nonPlain "abc") ])
  }

  let ``tupleを変換できる`` = test {
    let actual = represent (1, 3, "a")
    do! actual |> should equal (sequence [ (plain "1"); (plain "3"); (nonPlain "a") ])
  }

  let ``listを変換できる`` = test {
    let actual = represent [ "a"; "b"; "c" ]
    do! actual |> should equal (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c") ])
  }

  let ``arrayを変換できる`` = test {
    let actual = represent [| "a"; "b"; "c" |]
    do! actual |> should equal (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c") ])
  }

  let ``seqを変換できる`` = test {
    let actual = represent (seq { 1..3 })
    do! actual |> should equal (sequence [ (plain "1"); (plain "2"); (plain "3") ])
  }

  let ``setを変換できる`` = test {
    let actual = represent (set [ 1; 2; 3 ])
    do! actual |> should equal (sequence [ (plain "1"); (plain "2"); (plain "3") ])
  }

  let ``Mapを変換できる`` = test {
    let actual = represent (Map.ofList [ ("a", 1); ("b", 2) ])
    do! actual |> should equal (mapping [ (nonPlain "a", plain "1"); (nonPlain "b", plain "2") ])
  }

  let ``Option.Someを変換できる`` = test {
    let actual = represent (Some "abc")
    do! actual |> should equal (nonPlain "abc")
  }

  let ``Option.Noneを変換できる`` = test {
    let actual = represent (None: int option)
    do! actual |> should equal null'
  }
  
module DumpRecordTest =
  open System

  let representOmittingDefaultFields<'a> value = Native.represent<'a> (TypeDefinitions.recordDefOmittingDefaultFields :: TypeDefinitions.defaultDefinitions) value

  type TestRecord =
    { FieldA: int; FieldB: option<int> }
  with
    static member DefaultFieldA = -1

  let ``デフォルトでは省略可能なフィールドも出力される`` = test {
    let actual = represent { FieldA = -1; FieldB = None }
    do! actual |> should equal (mapping [(plain "FieldA", plain "-1"); (plain "FieldB", null')])
  }

  let ``省略可能なフィールドを出力しない定義が機能する`` = test {
      let actual = representOmittingDefaultFields { FieldA = -1; FieldB = None }
      do! actual |> should equal (mapping [ ])
    }

module DumpUnionTest =
  type TestUnion =
    | NoValue
    | OneField of int
    | ManyFields of int * int
    | OneNamedField of fieldA: int
    | NamedFields of fieldA: int * fieldB: string

  let ``値がないケースを変換できる`` = test {
    let actual = represent NoValue
    do! actual |> should equal (plain "NoValue")
  }

  let ``値が1つのケースを変換できる`` = test {
    let actual = represent (OneField 23)
    do! actual |> should equal (mapping [ (plain "OneField", plain "23") ])
  }

  let ``値が複数のケースを変換できる`` = test {
    let actual = represent (ManyFields (1, 3))
    do! actual |> should equal (mapping [ (plain "ManyFields", sequence [ (plain "1"); (plain "3") ])])
  }

  let ``値が1つの名前付きフィールドを変換できる`` = test {
    let actual = represent (OneNamedField (fieldA = 1))
    do! actual |> should equal (mapping [ (plain "OneNamedField", mapping [ (plain "fieldA", plain "1") ]) ])
  }

  let ``値が複数の名前付きフィールドを変換できる`` = test {
    let actual = represent (NamedFields (fieldA = 3, fieldB = "abc"))
    do! actual |> should equal (mapping [ (plain "NamedFields", mapping [ (plain "fieldA", plain "3"); (plain "fieldB", nonPlain "abc") ]) ])
  }