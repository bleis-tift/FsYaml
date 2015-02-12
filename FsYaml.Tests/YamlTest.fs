[<NUnit.Framework.TestFixture>]
module YamlTest

open FsUnit
open NUnit.Framework

open FsYaml

type ComplexType = {
  FieldA: int list
  FieldB: Map<int, int>
  FieldC: string
  FieldD: ComplexType option
}

let value = {
  FieldA = [ 1; 2; 3 ]
  FieldB = Map.ofList [ (100, 200); (200, 400); (500, 1000); (1, 2) ]
  FieldC = "abc\r\ndef"
  FieldD =
    Some {
      FieldA = []
      FieldB = Map.ofList [ (500, 250); (400, 200) ]
      FieldC = "abcdefghijk"
      FieldD = None
    }
}

[<Test>]
let ``オブジェクトをYamlの文字列にできる``() =
  let actual = Yaml.dump value
  let expected = """FieldA:
- 1
- 2
- 3
FieldB:
  1: 2
  100: 200
  200: 400
  500: 1000
FieldC: "abc\r\ndef"
FieldD:
  FieldA: []
  FieldB:
    400: 200
    500: 250
  FieldC: "abcdefghijk"
  FieldD: null
"""
  actual |> should equal expected

[<Test>]
let ``dumpしてloadすると元の値になる``() =
  let yaml = Yaml.dump value
  let actual = Yaml.load<ComplexType> yaml
  actual |> should equal value