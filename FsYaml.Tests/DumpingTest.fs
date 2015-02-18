module DumpingTest

open NUnit.Framework
open FsUnit

open FsYaml
open FsYaml.RepresentationTypes

let represent<'a> value = Native.represent<'a> TypeDefinitions.defaultDefinitions value

let scalar x = Scalar (x, None)
let plain x = scalar (Plain x)
let nonPlain x = scalar (NonPlain x)
let sequence x = Sequence (x, None)
let mapping x = Mapping (Map.ofSeq x, None)
let null' = Null None

[<TestFixture>]
module DumpTest =
  open System

  [<Test>]
  let ``intを変換できる``() =
    let actual = represent 1
    actual |> should equal (plain "1")

  [<Test>]
  let ``int64を変換できる``() =
    let actual = represent 20L
    actual |> should equal (plain "20")

  [<TestCase(Double.NaN, ".NaN")>]
  [<TestCase(Double.PositiveInfinity, "+.inf")>]
  [<TestCase(Double.NegativeInfinity, "-.inf")>]
  [<TestCase(45.5, "45.5")>]
  let ``floatを変換できる`` (value: float) (expected: string) =
    let actual = represent value
    actual |> should equal (plain expected)

  [<Test>]
  let ``stringを変換できる``() =
    let actual = represent "this is text"
    actual |> should equal (nonPlain "this is text")

  [<Test>]
  let ``nullのstringを変換できる``() =
    let actual = represent (null: string)
    actual |> should equal null'

  [<TestCase(true, "true")>]
  [<TestCase(false, "false")>]
  let ``boolを変換できる`` (value: bool) (expected: string) =
    let actual = represent value
    actual |> should equal (plain expected)

  [<Test>]
  let ``decimalを変換できる``() =
    let actual = represent 30.5m
    actual |> should equal (plain "30.5")

  [<Test>]
  let ``DateTimeを変換できる``() =
    let actual = represent (DateTime.Parse("2015-02-12 11:22:33.111"))
    actual |> should equal (nonPlain "2015-02-12 11:22:33.111")

  [<Test>]
  let ``TimeSpanを変換できる``() =
    let actual = represent (TimeSpan.Parse("11:22:33.444"))
    actual |> should equal (nonPlain "11:22:33.444")

  type Record = { FieldA: int; FieldB: string }
  [<Test>]
  let ``recordを変換できる``() =
    let actual = represent { FieldA = 20; FieldB = "abc" }
    actual |> should equal (mapping [ (plain "FieldA", plain "20"); (plain "FieldB", nonPlain "abc") ])

  [<Test>]
  let ``tupleを変換できる``() =
    let actual = represent (1, 3, "a")
    actual |> should equal (sequence [ (plain "1"); (plain "3"); (nonPlain "a") ])

  [<Test>]
  let ``listを変換できる``() =
    let actual = represent [ "a"; "b"; "c" ]
    actual |> should equal (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c") ])

  [<Test>]
  let ``arrayを変換できる``() =
    let actual = represent [| "a"; "b"; "c" |]
    actual |> should equal (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c") ])

  [<Test>]
  let ``seqを変換できる``() =
    let actual = represent (seq { 1..3 })
    actual |> should equal (sequence [ (plain "1"); (plain "2"); (plain "3") ])

  [<Test>]
  let ``Mapを変換できる``() =
    let actual = represent (Map.ofList [ ("a", 1); ("b", 2) ])
    actual |> should equal (mapping [ (nonPlain "a", plain "1"); (nonPlain "b", plain "2") ])

  [<Test>]
  let ``Option.Someを変換できる``() =
    let actual = represent (Some "abc")
    actual |> should equal (nonPlain "abc")

  [<Test>]
  let ``Option.Noneを変換できる``() =
    let actual = represent (None: int option)
    actual |> should equal null'

[<TestFixture>]
module DumpUnionTest =
  type TestUnion =
    | NoValue
    | OneField of int
    | ManyFields of int * int
    | OneNamedField of fieldA: int
    | NamedFields of fieldA: int * fieldB: string

  [<Test>]
  let ``値がないケースを変換できる``() =
    let actual = represent NoValue
    actual |> should equal (plain "NoValue")

  [<Test>]
  let ``値が1つのケースを変換できる``() =
    let actual = represent (OneField 23)
    actual |> should equal (mapping [ (plain "OneField", plain "23") ])

  [<Test>]
  let ``値が複数のケースを変換できる``() =
    let actual = represent (ManyFields (1, 3))
    actual |> should equal (mapping [ (plain "ManyFields", sequence [ (plain "1"); (plain "3") ])])

  [<Test>]
  let ``値が1つの名前付きフィールドを変換できる``() =
    let actual = represent (OneNamedField (fieldA = 1))
    actual |> should equal (mapping [ (plain "OneNamedField", mapping [ (plain "fieldA", plain "1") ]) ])

  [<Test>]
  let ``値が複数の名前付きフィールドを変換できる``() =
    let actual = represent (NamedFields (fieldA = 3, fieldB = "abc"))
    actual |> should equal (mapping [ (plain "NamedFields", mapping [ (plain "fieldA", plain "3"); (plain "fieldB", nonPlain "abc") ]) ])