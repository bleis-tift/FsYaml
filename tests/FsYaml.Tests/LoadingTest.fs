module LoadingTest

open Persimmon
open UseTestNameByReflection
open Assertions

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

module RepresentationTest =
  let Plainをパースできる =
    let body input = test {
      let actual = parse input
      do! actual |> should equal (Scalar (Plain input, None))
    }
    parameterize {
      case ("abc")
      case ("1")
      case ("3:4")
      case ("3-4")
      run body
    }
    
  let NonPlainをパースできる =
    let body (input, expected) = test {
      let actual = parse input
      do! actual |> should equal (Scalar (NonPlain expected, None))
    }
    parameterize {
      case ("\"abc\"", "abc")
      case ("'abc'", "abc")
      run body
    }

  let Sequenceをパースできる =
    let body input = test {
      let actual = parse input
      let expected = Sequence ([ Scalar (Plain "abc", None); Scalar (Plain "def", None); Scalar (NonPlain "ghi", None) ], None)
      do! actual |> should equal expected
    }
    parameterize {
      case """[ abc, def, "ghi" ]"""
      case """
- abc
- def
- "ghi"
"""
      run body
    }
    
  let Mappingをパースできる =
    let body input = test {
      let actual = parse input
      let expected =
        let mapping = Map.ofList [ Scalar (Plain "abc", None), Scalar (Plain "def", None);
                                   Scalar (Plain "ghi", None), Sequence ([ Scalar (Plain "jkf", None) ], None) ]
        Mapping (mapping, None)
      do! actual |> should equal expected
    }
    parameterize {
      case """{ abc: def, ghi: [ jkf ] }"""
      case """
abc: def
ghi:
     - jkf
"""
      run body
    }
    
  let Nullをパースできる =
    let body input = test {
      let actual = parse input
      let expected = Mapping (Map.ofList [ Scalar (Plain "abc", None), Null None ], None)
      do! actual |> should equal expected
    }
    parameterize {
      case "abc: "
      case "abc: null"
      case "abc: ~"
      run body
    }
    
module LoadTest =
  open System

  type UnknownType() = class end

  let 登録されていない型は例外 = test {
    let yaml = "1"
    let! e = trap { it (Yaml.load<UnknownType> yaml) }
    do! e.GetType() |> should equal typeof<FsYamlException>
  }

  let intに変換できる = test {
    let yaml = "1"
    let actual = Yaml.load<int> yaml
    do! actual |> should equal (int yaml)
  }

  let 変換できない場合は例外 = test {
    let yaml = "not int"
    let! e = trap { it (Yaml.load<int> yaml) }
    do! e.GetType() |> should equal typeof<FsYamlException>
  }

  let int64に変換できる = test {
    let yaml = "234"
    let actual = Yaml.load<int64> yaml
    do! actual |> should equal (int64 yaml)
  }

  let floatに変換できる =
    let body (yaml, expected) = test {
      let actual = Yaml.load<float> yaml
      do! actual |> should equalFloat expected
    }
    parameterize {
      case ("234.5", 234.5)
      case (".inf", Double.PositiveInfinity)
      case ("+.inf", Double.PositiveInfinity)
      case ("-.inf", Double.NegativeInfinity)
      case (".nan", Double.NaN)
      run body
    }

  let stringに変換できる =
    let body (yaml, expected) = test {
      let actual = Yaml.load<string> yaml
      do! actual |> should equal expected
    }
    parameterize {
      case ("aaaaa", "aaaaa")
      case ("''", "")
      case ("~", null)
      case ("null", null)
      case ("'null'", "null")
      run body
    }

  let boolに変換できる =
    let body (yaml, expected) = test {
      let actual = Yaml.load<bool> yaml
      do! actual |> should equal expected
    }
    parameterize {
      case ("true", true)
      case ("yes", true)
      case ("y", true)
      case ("on", true)
      case ("FALSE", false)
      case ("NO", false)
      case ("N", false)
      case ("OFF", false)
      run body
    }

  let decimalに変換できる = test {
    let yaml = "100.5"
    let actual = Yaml.load<decimal> yaml
    do! actual |> should equal (decimal yaml)
  }

  let DateTimeに変換できる = test {
    let yaml =" 2015-2-5 22:00:01.222"
    let actual = Yaml.load<DateTime> yaml
    do! actual |> should equal (DateTime.Parse(yaml))
  }

  let TimeSpanに変換できる = test {
    let yaml = "01:22:33.444"
    let actual = Yaml.load<TimeSpan> yaml
    do! actual |> should equal (TimeSpan.Parse(yaml))
  }

  let tupleに変換できる = test {
    let yaml = "[ 1, 2, 3, abc ]"
    let actual = Yaml.load<int * int * string * string> yaml
    do! actual |> should equal (1, 2, "3", "abc")
  }

  let tupleの要素が不足している場合は失敗する = test {
    let yaml = "[ 1, 2, 3 ]"
    let! e = trap { it (Yaml.load<int * int * int * int> yaml) }
    do! e.GetType() |> should equal typeof<FsYamlException>
  }

  let tupleの要素が多い場合は失敗する = test {
    let yaml = "[ 1, 2, 3, 4, 5 ]"
    let! e = trap { it (Yaml.load<int * int * int * int> yaml) }
    do! e.GetType() |> should equal typeof<FsYamlException>
  }

  let listに変換できる = test {
    let yaml = "[ a, b, c, d, e ]"
    let actual = Yaml.load<string list> yaml
    do! actual |> should equal [ "a"; "b"; "c"; "d"; "e" ]
  }

  let 空のリストに変換できる = test {
    let yaml = "[]"
    let actual = Yaml.load<string list> yaml
    do! actual |> should equal ([]: string list)
  }

  let setに変換できる = test {
    let yaml = "[ 1, 2, 3 ]"
    let actual = Yaml.load<Set<int>> yaml
    do! actual |> should equal (set [1; 2; 3])
  }

  let mapに変換できる = test {
    let yaml = "{ a: 1, b: 2 }"
    let actual = Yaml.load<Map<string, int>> yaml
    do! actual |> should equal (Map.ofList [ ("a", 1); ("b", 2) ])
  }

  let 空のmapに変換できる = test {
    let yaml = "{}"
    let actual = Yaml.load<Map<string, int>> yaml
    do! actual |> should equal (Map.empty<string, int>)
  }

  let arrayに変換できる = test {
    let yaml = "[ 1, 2, 3 ]"
    let actual = Yaml.load<int[]> yaml
    do! actual |> should equal ([| 1; 2; 3 |])
  }

  let 空のarrayに変換できる = test {
    let yaml = "[]"
    let actual = Yaml.load<int[]> yaml
    do! actual |> should equal (Array.empty<int>)
  }

  let seqに変換できる = test {
    let yaml = "[ 1, 2, 3 ]"
    let actual = Yaml.load<seq<int>> yaml
    do! actual |> should equalSeq (seq { 1..3 })
  }

  let 空のseqに変換できる = test {
    let yaml = "[]"
    let actual = Yaml.load<seq<int>> yaml
    do! actual |> should equalSeq (Seq.empty<int>)
  }

module LoadUnionTest =
  type OneCase = OneCase

  let ``ケース1つ、フィールドなし`` = test {
    let yaml = "OneCase"
    let actual = Yaml.load<OneCase> yaml
    do! actual |> should equal OneCase
  }

  type TwoCase = TwoCase_1 | TwoCase_2

  let ``ケース2つ、フィールドなし`` = test {
    let yaml = "TwoCase_2"
    let actual = Yaml.load<TwoCase> yaml
    do! actual |> should equal TwoCase_2
  }

  type OneFieldCase = OneFieldCase of int

  let ``フィールド1つ`` = test {
    let yaml = "OneFieldCase: 1"
    let actual = Yaml.load<OneFieldCase> yaml
    do! actual |> should equal (OneFieldCase 1)
  }

  type TupleFieldCase = TupleFieldCase of (int * string)

  let ``フィールドがタプル`` = test {
    let yaml = "TupleFieldCase: [ 1, a ]"
    let actual = Yaml.load<TupleFieldCase> yaml
    do! actual |> should equal (TupleFieldCase (1, "a"))
  }

  type ListFieldCase = ListFieldCase of int list

  let ``フィールドがリスト`` = test {
    let yaml = "ListFieldCase: [ 1, 2, 3 ]"
    let actual = Yaml.load<ListFieldCase> yaml
    do! actual |> should equal (ListFieldCase [ 1; 2; 3 ])
  }

  type MapFieldCase = MapFieldCase of Map<string, int>

  let ``フィールドがMap`` = test {
    let yaml = "MapFieldCase: { a: 1, b: 2 }"
    let actual = Yaml.load<MapFieldCase> yaml
    do! actual |> should equal (MapFieldCase (Map.ofList [ ("a", 1); ("b", 2) ]))
  }

  type ManyFieldsCase = ManyFieldsCase of int * string * string
  
  let ``フィールドが複数あるケース`` = test {
    let yaml = "ManyFieldsCase: [ 1, a, b ]"
    let actual = Yaml.load<ManyFieldsCase> yaml
    do! actual |> should equal (ManyFieldsCase (1, "a", "b"))
  }

  type OneNamedFieldCase = OneNamedFieldCase of x: int

  let ``1つの名前付きフィールドのケース`` = test {
    let yaml = "OneNamedFieldCase: { x: 1 }"
    let actual = Yaml.load<OneNamedFieldCase> yaml
    do! actual |> should equal (OneNamedFieldCase (x = 1))
  }

  type ManyNamedFieldCase = ManyNamedFieldCase of x: int * y: string
  
  let ``複数の名前付きフィールドのケース`` = test {
    let yaml = "ManyNamedFieldCase: { x: 1, y: a }"
    let actual = Yaml.load<ManyNamedFieldCase> yaml
    do! actual |> should equal (ManyNamedFieldCase (x = 1, y = "a"))
  }

  let ``１つの暗黙の名前でも変換できる`` = test {
    let yaml = "OneFieldCase: { Item: 1 }"
    let actual = Yaml.load<OneFieldCase> yaml
    do! actual |> should equal (OneFieldCase 1)
  }

  let ``複数の暗黙の名前でも変換できる`` = test {
    let yaml = "ManyFieldsCase: { Item1: 1, Item2: a, Item3: b }"
    let actual = Yaml.load<ManyFieldsCase> yaml
    do! actual |> should equal (ManyFieldsCase (1, "a", "b"))
  }

  type HalfNamedFieldCase =
    | HalfNamedFieldCaseA of x: int * int
    | HalfNamedFieldCaseB of int * y: int

  let ``一つ目の要素のみ名前がついたケースを変換できる`` = test {
    let yaml = "HalfNamedFieldCaseA: { x: 1, Item2: 2 }"
    let actual = Yaml.load<HalfNamedFieldCase> yaml
    do! actual |> should equal (HalfNamedFieldCaseA (1, 2))
  }

  let ``ニつ目の要素のみ名前がついたケースを変換できる`` = test {
    let yaml = "HalfNamedFieldCaseB: { Item1: 1, y: 2 }"
    let actual = Yaml.load<HalfNamedFieldCase> yaml
    do! actual |> should equal (HalfNamedFieldCaseB (1, 2))
  }

  [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
  type UseNullAsTrueValueCase = NullCase | ValueCase1 of int | ValueCase2 of string
    
  let ``UseNullAsTrueValueのnullのケース`` = test {
    let yaml = "NullCase"
    let actual = Yaml.load<UseNullAsTrueValueCase> yaml
    do! actual |> should equal NullCase
  }

  let ``UseNullAsTrueValueCaseの値があるケース`` = test {
    let yaml = "ValueCase2: a"
    let actual = Yaml.load<UseNullAsTrueValueCase> yaml
    do! actual |> should equal (ValueCase2 "a")
  }

  let ``Option.None`` =
    let body yaml = test {
      let actual = Yaml.load<Option<int>> yaml
      do! actual |> should equal (None: int option)
    }
    parameterize {
      case ("~")
      case ("null")
      case ("None")
      run body
    }
    
  let ``Option.Some`` =
    let body yaml = test {
      let actual = Yaml.load<Option<int>> yaml
      do! actual |> should equal (Some 1)
    }
    parameterize {
      case ("Some: 1")
      case ("1")
      run body
    }

  module LoadInferUnionCaseTest =
    open FsYaml.Attributes
    open FsYaml.Utility

    [<InferUnionCase>]
    type InferUnion =
      | Int of int
      | String of string
      | Tuple of int * string
      | List of list<string>
      | MapInt of Map<int, string>
      | MapString of Map<string, string>

    let ``ユニオンケース推論ができる`` =
      let body (yaml, expected) = test {
        let actual = yaml |> Yaml.load<InferUnion>
        do! actual |> should equal expected
      }
      parameterize {
        // 標準的なテスト
        case ("a", String "a")
        case ("1.2", String "1.2")
        case ("[1, a]", Tuple (1, "a"))
        case ("[a, b]", List ["a"; "b"])
        case ("[1, 2, 3]", List ["1"; "2"; "3"])
        case ("{a: b}", MapString (Map.ofList [("a", "b")]))
        // plain スカラーは非文字列への変換が優先される
        case ("1", Int 1)
        case ("[1, 'a']", Tuple (1, "a"))
        case ("{1: a}", MapInt (Map.ofList [(1, "a")]))
        // non-plain スカラーは文字列への変換が優先される
        case ("'1'", String "1")
        case ("['1', 'a']", List ["1"; "a"])
        case ("{'1': a}", MapString (Map.ofList [("1", "a")]))
        run body
      }

    [<InferUnionCase>]
    type BinaryTree =
      | Leaf of int
      | Node of BinaryTree * BinaryTree
      
    let ``再帰的なユニオンケース推論ができる`` =
      let body (yaml, expected) = test {
        let actual = yaml |> Yaml.load<BinaryTree>
        do! actual |> should equal expected
      }
      parameterize {
        // 標準的なテスト
        case ("1", Leaf 1)
        case ("[1, 2]", Node (Leaf 1, Leaf 2))
        case ("[1, [2, 3]]", Node (Leaf 1, Node(Leaf 2, Leaf 3)))
        // ユニオンケースつきでも推論できる
        case ("Leaf: 1", Leaf 1)
        case ("[1, Leaf: 2]", Node (Leaf 1, Leaf 2))
        case ("Node: [1, Node: [2, 3]]", Node (Leaf 1, Node(Leaf 2, Leaf 3)))
        run body
      }

    [<InferUnionCase>]
    type InferNull = InferNull

    let ``値なしユニオンケースを推論できる`` = test {
      let actual = Yaml.load<InferNull> "null"
      do! actual |> should equal InferNull
    }

    [<InferUnionCase>]
    type InferInt = InferInt of int

    [<InferUnionCase>]
    type InferString = InferString of string

    let ``好ましくない変換も一意なら成功する`` =
      test {
        do! Yaml.load<InferInt> "'1'" |> should equal (InferInt 1)
        do! Yaml.load<InferString> "1" |> should equal (InferString "1")
      }

    [<InferUnionCase>]
    type ComplexInferUnion =
      | InferIntA of int
      | InferIntB of int
      | InferIntInt of int * int
      | InferStringString of string * string

    let ``ユニオンケース推論の競合を検出できる`` =
      let body yaml = test {
        let! _ = trap { it (yaml |> Yaml.load<ComplexInferUnion>) }
        return ()
      }
      parameterize {
        // 候補なし
        case "a"
        case "[]"
        // 競合
        case "1"
        case "'1'"
        case "['1', 2]"
        case "[1, '2']"
        run body
      }

    [<InferUnionCase>]
    type InferMap = InferMap of Map<string, InferMap>

    let InferMapTest = test {
      do! Yaml.load<InferMap> "InferMap: {}" |> should equal (InferMap Map.empty)
    }

    let ``ユニオンケースの有無による競合を検出できる`` = test {
      let! _ = trap { it (Yaml.load<InferMap> "'InferMap': {}") }
      return ()
    }

module LoadRecordTest =
  type TestRecord = { A: int; B: string }

  let recordに変換できる = test {
    let yaml = "{ A: 123, B: abc }"
    let actual = Yaml.load<TestRecord> yaml
    do! actual |> should equal { A = 123; B = "abc" }
  }

  let recordの項目が足りない場合は失敗する = test {
    let yaml = "{ A: 123 }"
    let! e = trap { it (Yaml.load<TestRecord> yaml) }
    do! e.GetType() |> should equal typeof<FsYamlException>
  }

  module OptionalMemberTest =
    module StaticDefaultValueField =
      type WithDefaultMember = {
        A: string
        B: int
      }
      with
        static member DefaultB = 3

      let DefaultValueが定義されている = test {
        let yaml = "{ A: abc }"
        let actual = Yaml.load<WithDefaultMember> yaml
        do! actual |> should equal { A = "abc"; B = 3 }
      }

      type InvalidType = {
        A: string
        B: int
      }
      with
        static member DefaultB = "3"

      let DefaultValueの型が違う場合は例外 = test {
        let yaml = "{ A: abc }"
        let! e = trap { it (Yaml.load<InvalidType> yaml) }
        do! e.GetType() |> should equal typeof<FsYamlException>
      }

    module OptionTypeField =
      open System

      type WithOption = {
        A: int
        B: int option
      }

      let 省略された場合Noneになる = test {
        let yaml = "{ A: 1 }"
        let actual = Yaml.load<WithOption> yaml
        do! actual |> should equal { A = 1; B = None }
      }
   
module LoadCustomTypeTest =
  open FsYaml.NativeTypes
  open FsYaml.CustomTypeDefinition
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

  let ``ユーザが作成した型をloadできる`` = test {
    let yaml = "1"
    let actual = Yaml.loadWith<CustomType> [ customConstructor ] yaml
    do! actual |> should equal (CustomType(1))
  }

module LoadComplexTypeTest =
  type t = Case1 | Case2 of int list

  let ``複雑な型をloadできる`` = test {
    let yaml = "- Case1
- Case2: [ 42 ]"
    let actual = Yaml.load<t list> yaml
    do! actual |> should equal [ Case1; Case2 [ 42 ] ]
  }