module YamlTest

open Persimmon
open UseTestNameByReflection
open Assertions

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

let ``オブジェクトをYamlの文字列にできる`` = test {
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
  do! actual |> should equal expected
}

let ``dumpしてloadすると元の値になる`` = test {
  let yaml = Yaml.dump value
  let actual = Yaml.load<ComplexType> yaml
  do! actual |> should equal value
}