module LoadingTest

open NUnit.Framework
open FsUnit

open FsYaml
open FsYaml.RepresentationTypes

let rec clearPosition = function
  | Scalar (v, _) -> Scalar (v, None)
  | Sequence (v, _) ->
    let values = List.map clearPosition v
    Sequence (values, None)
  | Mapping (v, _) ->
    let mapping = v |> Seq.map (fun (KeyValue(key, value)) -> (clearPosition key, clearPosition value)) |> Map.ofSeq
    Mapping (mapping, None)
  | Null _ -> Null None

let parse = FsYaml.Representation.parse >> clearPosition

[<TestFixture>]
module RepresentationTest =
  [<TestCase("abc")>]
  [<TestCase("1")>]
  [<TestCase("3:4")>]
  [<TestCase("3-4")>]
  let Plainをパースできる input =
    let actual = parse input
    actual |> should equal (Scalar (Plain input, None))

  [<TestCase("\"abc\"", "abc")>]
  [<TestCase("'abc'", "abc")>]
  let NonPlainをパースできる input expected =
    let actual = parse input
    actual |> should equal (Scalar (NonPlain expected, None))


  [<TestCase("""[ abc, def, "ghi" ]""")>]
  [<TestCase("""
- abc
- def
- "ghi"
""")>]
  let Sequenceをパースできる input =
    let actual = parse input
    let expected = Sequence ([ Scalar (Plain "abc", None); Scalar (Plain "def", None); Scalar (NonPlain "ghi", None) ], None)
    actual |> should equal expected

  [<TestCase("""{ abc: def, ghi: [ jkf ] }""")>]
  [<TestCase("""
abc: def
ghi:
     - jkf
""")>]
  let Mappingをパースできる input =
    let actual = parse input
    let expected =
      let mapping = Map.ofList [ Scalar (Plain "abc", None), Scalar (Plain "def", None);
                                 Scalar (Plain "ghi", None), Sequence ([ Scalar (Plain "jkf", None) ], None) ]
      Mapping (mapping, None)
    actual |> should equal expected

  [<TestCase("abc: ")>]
  [<TestCase("abc: null")>]
  [<TestCase("abc: ~")>]
  let Nullをパースできる input =
    let actual = parse input
    let expected = Mapping (Map.ofList [ Scalar (Plain "abc", None), Null None ], None)
    actual |> should equal expected

[<TestFixture>]
module LoadTest =
  open System

  type UnknownType() = class end

  [<Test>]
  [<ExpectedException(typeof<FsYamlException>)>]
  let 登録されていない型は例外() =
    let yaml = "1"
    Yaml.load<UnknownType> yaml |> ignore

  [<TestCase("1")>]
  let intに変換できる yaml =
    let actual = Yaml.load<int> yaml
    actual |> should equal (int yaml)

  [<Test>]
  [<ExpectedException>]
  let 変換できない場合は例外() =
    let yaml = "not int"
    Yaml.load<int> yaml |> ignore

  [<TestCase("234")>]
  let int64に変換できる yaml =
    let actual = Yaml.load<int64> yaml
    actual |> should equal (int64 yaml)

  [<TestCase("234.5", 234.5)>]
  [<TestCase(".inf", Double.PositiveInfinity)>]
  [<TestCase("+.inf", Double.PositiveInfinity)>]
  [<TestCase("-.inf", Double.NegativeInfinity)>]
  [<TestCase(".nan", Double.NaN)>]
  let floatに変換できる yaml (expected: float)=
    let actual = Yaml.load<float> yaml
    actual |> should equal (expected)

  [<TestCase("aaaaa", "aaaaa")>]
  [<TestCase("''", "")>]
  [<TestCase("~", null)>]
  [<TestCase("null", null)>]
  [<TestCase("'null'", "null")>]
  let stringに変換できる yaml (expected: string) =
    let actual = Yaml.load<string> yaml
    actual |> should equal expected

  [<TestCase("true", true)>]
  [<TestCase("yes", true)>]
  [<TestCase("y", true)>]
  [<TestCase("on", true)>]
  [<TestCase("FALSE", false)>]
  [<TestCase("NO", false)>]
  [<TestCase("N", false)>]
  [<TestCase("OFF", false)>]
  let boolに変換できる yaml (expected: bool) =
    let actual = Yaml.load<bool> yaml
    actual |> should equal expected

  [<TestCase("100.5")>]
  let decimalに変換できる yaml =
    let actual = Yaml.load<decimal> yaml
    actual |> should equal (decimal yaml)

  [<TestCase("2015-2-5 22:00:01.222")>]
  let DateTimeに変換できる yaml =
    let actual = Yaml.load<DateTime> yaml
    actual |> should equal (DateTime.Parse(yaml))

  [<TestCase("01:22:33.444")>]
  let TimeSpanに変換できる yaml =
    let actual = Yaml.load<TimeSpan> yaml
    actual |> should equal (TimeSpan.Parse(yaml))

  type TestRecord = { A: int; B: string }

  [<Test>]
  let recordに変換できる() =
    let yaml = "{ A: 123, B: abc }"
    let actual = Yaml.load<TestRecord> yaml
    actual |> should equal { A = 123; B = "abc" }

  [<Test>]
  [<ExpectedException(typeof<FsYamlException>)>]
  let recordの項目が足りない場合は失敗する() =
    let yaml = "{ A: 123 }"
    Yaml.load<TestRecord> yaml |> ignore

  [<Test>]
  let tupleに変換できる() =
    let yaml = "[ 1, 2, 3, abc ]"
    let actual = Yaml.load<int * int * string * string> yaml
    actual |> should equal (1, 2, "3", "abc")

  [<Test>]
  [<ExpectedException(typeof<FsYamlException>)>]
  let tupleの要素が不足している場合は失敗する () =
    let yaml = "[ 1, 2, 3 ]"
    Yaml.load<int * int * int * int> yaml |> ignore

  [<Test>]
  [<ExpectedException(typeof<FsYamlException>)>]
  let tupleの要素が多い場合は失敗する () =
    let yaml = "[ 1, 2, 3, 4, 5 ]"
    Yaml.load<int * int * int * int> yaml |> ignore

  [<Test>]
  let listに変換できる() =
    let yaml = "[ a, b, c, d, e ]"
    let actual = Yaml.load<string list> yaml
    actual |> should equal [ "a"; "b"; "c"; "d"; "e" ]

  [<Test>]
  let 空のリストに変換できる() =
    let yaml = "[]"
    let actual = Yaml.load<string list> yaml
    actual |> should equal ([]: string list)

  [<Test>]
  let mapに変換できる() =
    let yaml = "{ a: 1, b: 2 }"
    let actual = Yaml.load<Map<string, int>> yaml
    actual |> should equal (Map.ofList [ ("a", 1); ("b", 2) ])

  [<Test>]
  let 空のmapに変換できる() =
    let yaml = "{}"
    let actual = Yaml.load<Map<string, int>> yaml
    actual |> should equal (Map.empty<string, int>)

  [<Test>]
  let arrayに変換できる() =
    let yaml = "[ 1, 2, 3 ]"
    let actual = Yaml.load<int[]> yaml
    actual |> should equal ([| 1; 2; 3 |])

  [<Test>]
  let 空のarrayに変換できる() =
    let yaml = "[]"
    let actual = Yaml.load<int[]> yaml
    actual |> should equal (Array.empty<int>)

  [<Test>]
  let seqに変換できる() =
    let yaml = "[ 1, 2, 3 ]"
    let actual = Yaml.load<seq<int>> yaml
    actual |> should equal (seq { 1..3 })

  [<Test>]
  let 空のseqに変換できる() =
    let yaml = "[]"
    let actual = Yaml.load<seq<int>> yaml
    actual |> should equal (Seq.empty<int>)

[<TestFixture>]
module LoadUnionTest =
  type OneCase = OneCase

  [<Test>]
  let ``ケース1つ、フィールドなし``() =
    let yaml = "OneCase"
    let actual = Yaml.load<OneCase> yaml
    actual |> should equal OneCase

  type TwoCase = TwoCase_1 | TwoCase_2

  [<Test>]
  let ``ケース2つ、フィールドなし``() =
    let yaml = "TwoCase_2"
    let actual = Yaml.load<TwoCase> yaml
    actual |> should equal TwoCase_2

  type OneFieldCase = OneFieldCase of int

  [<Test>]
  let ``フィールド1つ``() =
    let yaml = "OneFieldCase: 1"
    let actual = Yaml.load<OneFieldCase> yaml
    actual |> should equal (OneFieldCase 1)

  type TupleFieldCase = TupleFieldCase of (int * string)

  [<Test>]
  let ``フィールドがタプル``() =
    let yaml = "TupleFieldCase: [ 1, a ]"
    let actual = Yaml.load<TupleFieldCase> yaml
    actual |> should equal (TupleFieldCase (1, "a"))

  type ListFieldCase = ListFieldCase of int list

  [<Test>]
  let ``フィールドがリスト``() =
    let yaml = "ListFieldCase: [ 1, 2, 3 ]"
    let actual = Yaml.load<ListFieldCase> yaml
    actual |> should equal (ListFieldCase [ 1; 2; 3 ])

  type MapFieldCase = MapFieldCase of Map<string, int>

  [<Test>]
  let ``フィールドがMap``() =
    let yaml = "MapFieldCase: { a: 1, b: 2 }"
    let actual = Yaml.load<MapFieldCase> yaml
    actual |> should equal (MapFieldCase (Map.ofList [ ("a", 1); ("b", 2) ]))

  type ManyFieldsCase = ManyFieldsCase of int * string * string
  
  [<Test>]
  let ``フィールドが複数あるケース``() =
    let yaml = "ManyFieldsCase: [ 1, a, b ]"
    let actual = Yaml.load<ManyFieldsCase> yaml
    actual |> should equal (ManyFieldsCase (1, "a", "b"))

  type OneNamedFieldCase = OneNamedFieldCase of x: int

  [<Test>]
  let ``1つの名前付きフィールドのケース``() =
    let yaml = "OneNamedFieldCase: { x: 1 }"
    let actual = Yaml.load<OneNamedFieldCase> yaml
    actual |> should equal (OneNamedFieldCase (x = 1))

  type ManyNamedFieldCase = ManyNamedFieldCase of x: int * y: string
  
  [<Test>]
  let ``複数の名前付きフィールドのケース``() =
    let yaml = "ManyNamedFieldCase: { x: 1, y: a }"
    let actual = Yaml.load<ManyNamedFieldCase> yaml
    actual |> should equal (ManyNamedFieldCase (x = 1, y = "a"))

  [<Test>]
  let ``１つの暗黙の名前でも変換できる``() =
    let yaml = "OneFieldCase: { Item: 1 }"
    let actual = Yaml.load<OneFieldCase> yaml
    actual |> should equal (OneFieldCase 1)

  [<Test>]
  let ``複数の暗黙の名前でも変換できる``() =
    let yaml = "ManyFieldsCase: { Item1: 1, Item2: a, Item3: b }"
    let actual = Yaml.load<ManyFieldsCase> yaml
    actual |> should equal (ManyFieldsCase (1, "a", "b"))

  type HalfNamedFieldCase =
    | HalfNamedFieldCaseA of x: int * int
    | HalfNamedFieldCaseB of int * y: int

  [<Test>]
  let ``一つ目の要素のみ名前がついたケースを変換できる``() =
    let yaml = "HalfNamedFieldCaseA: { x: 1, Item2: 2 }"
    let actual = Yaml.load<HalfNamedFieldCase> yaml
    actual |> should equal (HalfNamedFieldCaseA (1, 2))

  [<Test>]
  let ``ニつ目の要素のみ名前がついたケースを変換できる``() =
    let yaml = "HalfNamedFieldCaseB: { Item1: 1, y: 2 }"
    let actual = Yaml.load<HalfNamedFieldCase> yaml
    actual |> should equal (HalfNamedFieldCaseB (1, 2))

  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  type UseNullAsTrueValueCase = NullCase | ValueCase1 of int | ValueCase2 of string
    
  [<Test>]
  let ``UseNullAsTrueValueのnullのケース``() =
    let yaml = "NullCase"
    let actual = Yaml.load<UseNullAsTrueValueCase> yaml
    actual |> should equal NullCase

  [<Test>]
  let ``UseNullAsTrueValueCaseの値があるケース``() =
    let yaml = "ValueCase2: a"
    let actual = Yaml.load<UseNullAsTrueValueCase> yaml
    actual |> should equal (ValueCase2 "a")

  [<TestCase("~")>]
  [<TestCase("null")>]
  [<TestCase("None")>]
  let ``Option.None`` yaml =
    let actual = Yaml.load<Option<int>> yaml
    actual |> should equal (None: int option)

  [<TestCase("Some: 1")>]
  [<TestCase("1")>]
  let ``Option.Some`` yaml =
    let actual = Yaml.load<Option<int>> yaml
    actual |> should equal (Some 1)

[<TestFixture>]
module LoadCustomTypeTest =
  open FsYaml.NativeTypes
  open System

  type CustomType(x: int) =
    member val X = x

    override this.Equals(other) =
      if other.GetType() = typeof<CustomType> then
        let other = unbox<CustomType> other
        this.X = other.X
      else
        false

    override this.GetHashCode() = this.X.GetHashCode()

  let customConstructor = { 
    Accept = ((=) typeof<CustomType>)
    Construct = fun construct' t yaml ->
      match yaml with
      | Scalar _ as x ->
        let value = construct' typeof<int> x :?> int
        CustomType(value) |> box
      | _ -> null
    Represent = fun _ -> raise (NotImplementedException())
  }

  [<Test>]
  let ``ユーザが作成した型をloadできる``() =
    let yaml = "1"
    let actual = Yaml.loadWith<CustomType> [ customConstructor ] yaml
    actual |> should equal (CustomType(1))