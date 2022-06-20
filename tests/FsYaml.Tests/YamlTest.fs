module Tests

open Expecto
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

[<Tests>]
let tests =
  testList "samples" [
    test "Objects can be Yaml strings" {
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

    "Objects should be equal" |> Expect.equal actual expected
    }
    
    test "If you dump and load it, it will return to the original value." {
      let yaml = Yaml.dump value
      let actual = Yaml.load<ComplexType> yaml
      "Objects should be equal" |> Expect.equal actual value
    }
  ]
