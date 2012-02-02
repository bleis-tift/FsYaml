module YamlScenario

open NUnit.Framework
open NaturalSpec

module int =
  [<Scenario>]
  let ``42というYAML文字列を42に変換できる``() =
    Given "# first line comment\n42"
    |> When Yaml.load<int>
    |> It should equal 42
    |> Verify

// loadにstringを直接指定すると、何でも食べる
module string =
  [<Scenario>]
  let ``42というYAML文字列を"42"に変換できる``() =
    Given "42"
    |> When Yaml.load<string>
    |> It should equal "42"
    |> Verify

  [<Scenario>]
  let ``hogeというYAML文字列を"hoge"に変換できる``() =
    Given "hoge"
    |> When Yaml.load<string>
    |> It should equal "hoge"
    |> Verify

  [<Scenario>]
  let ``[1, 2, 3]というYAML文字列を"[1, 2, 3]"に変換できる``() =
    Given "[1, 2, 3]"
    |> When Yaml.load<string>
    |> It should equal "[1, 2, 3]"
    |> Verify

module ``int list`` =
  [<Scenario>]
  let ``[1, 2, 3]というYAML文字列を[1; 2; 3]に変換できる``() =
    Given "[1, 2, 3]"
    |> When Yaml.load<int list>
    |> It should equal [1; 2; 3]
    |> Verify

  [<Scenario>]
  let ``[1, 2, 4]というYAML文字列を[1; 2; 4]に変換できる``() =
    Given "[1, 2, 4]"
    |> When Yaml.load<int list>
    |> It should equal [1; 2; 4]
    |> Verify

  [<Scenario>]
  let ``[ 10 ,2,3 ]というYAML文字列を[1; 2; 3]に変換できる``() =
    Given "[ 10 ,2,3 ]"
    |> When Yaml.load<int list>
    |> It should equal [10; 2; 3]
    |> Verify

  [<Scenario>]
  let ブロック形式のリストも扱える() =
    Given "- 10\n\
           - 20\t \n\
           - -10"
    |> When Yaml.load<int list>
    |> It should equal [10; 20; -10]
    |> Verify

module ``string list`` =
  [<Scenario>]
  let ``[ abc, def , ghi ]というYAML文字列を["abc"; "def"; "ghi"]に変換できる``() =
    Given "[ abc, def , ghi ]"
    |> When Yaml.load<string list>
    |> It should equal ["abc"; "def"; "ghi"]
    |> Verify

  [<Scenario>]
  let ブロック形式のリストも扱える() =
    Given "- hoge\n\
           - piyo\n\
           - foo"
    |> When Yaml.load<string list>
    |> It should equal ["hoge"; "piyo"; "foo"]
    |> Verify

module 単純なレコード =
  type t = { Name: string option; Age: int option }
  [<Scenario>]
  let ``{ Name: hoge piyo, Age: 20 }というYAML文字列を{ Name = Some "hoge piyo"; Age = Some 20 }に変換できる``() =
    Given "{ Name: hoge piyo, Age: 20 }"
    |> When Yaml.load<t>
    |> It should equal { Name = Some "hoge piyo"; Age = Some 20 }
    |> Verify

  [<Scenario>]
  let ブロック形式のレコードも扱える() =
    Given "Name: hoge piyo\n\
           Age: 20"
    |> When Yaml.load<t>
    |> It should equal { Name = Some "hoge piyo"; Age = Some 20 }
    |> Verify

module ``インラインのstring listを持つレコード`` =
  type t = { Name: string; Comunities: string list }
  [<Scenario>]
  let ``{ Name: aaa bbb, Comunities: [ 'F#', Scala ] }というYAML文字列を{ Name = "aaa bbb"; Comunities = ["F#"; "Scala"] }に変換できる``() =
    Given "{ Name: aaa bbb, Comunities: [ 'F#', Scala ] }"
    |> When Yaml.load<t>
    |> It should equal { Name = "aaa bbb"; Comunities = ["F#"; "Scala"] }
    |> Verify

  [<Scenario>]
  let ブロック形式のレコードも扱える() =
    Given "Name: aaa bbb\n\
           Comunities: ['F#', Scala]"
    |> When Yaml.load<t>
    |> It should equal { Name = "aaa bbb"; Comunities = ["F#"; "Scala"] }
    |> Verify

  [<Scenario>]
  let インデントを扱える() =
    Given "Name: aaa bbb\n\
           Comunities: - 'F#'\n" +
          "            - Scala\n" +
          "            - OCaml"
    |> When Yaml.load<t>
    |> It should equal { Name = "aaa bbb"; Comunities = [ "F#"; "Scala"; "OCaml" ] }
    |> Verify

module ``インラインのint listを持つレコード`` =
  type t = { Hoge: string; Piyo: int list }
  [<Scenario>]
  let ``{ Hoge: 42, Piyo: [ 1, 2, 3 ] }というYAML文字列を{ Hoge = "42"; Piyo = [1; 2; 3] }に変換できる``() =
    Given "{ Hoge: 42, Piyo: [ 1, 2, 3 ] }"
    |> When Yaml.load<t>
    |> It should equal { Hoge = "42"; Piyo = [ 1; 2; 3 ] }
    |> Verify

module ``リストのレコード`` =
  type s = { Name: string; Age: int }
  type t = s list
  [<Scenario>]
  let ``[ { Name: hoge piyo, Age: 20 }, { Name: foo bar, Age: 30 } ]というYAML文字列を[ { Name = "hoge piyo"; Age = 20 }; { Name = "foo bar"; Age = 30 } ]に変換できる``() =
    Given "[ { Name: hoge piyo, Age: 20 }, { Name: foo bar, Age: 30 } ]"
    |> When Yaml.load<t>
    |> It should equal [ { Name = "hoge piyo"; Age = 20 }; { Name = "foo bar"; Age = 30 } ]
    |> Verify

  [<Scenario>]
  let ブロックもOK() =
    Given "- { Name: hoge piyo, Age: 20 }\n\
           - { Name: foo bar, Age: 30 }"
    |> When Yaml.load<t>
    |> It should equal [ { Name = "hoge piyo"; Age = 20 }; { Name = "foo bar"; Age = 30 } ]
    |> Verify

module ``レコードのレコード`` =
  type s = { Name: string; Age: int }
  type t = { Person: s; Communities: string list }
  [<Scenario>]
  let インラインがOK() =
    Given "{ Person: { Name: hoge piyo, Age: 20 }, Communities: ['F#', Scala] }"
    |> When Yaml.load<t>
    |> It should equal { Person = { Name = "hoge piyo"; Age = 20 }; Communities = ["F#"; "Scala"] }
    |> Verify

  [<Scenario>]
  let ブロックもOK() =
    Given "Person: { Name: hoge piyo, Age: 20 }\n\
           Communities: ['F#', Scala]"
    |> When Yaml.load<t>
    |> It should equal { Person = { Name = "hoge piyo"; Age = 20 }; Communities = ["F#"; "Scala"] }
    |> Verify

module ``レコードのレコードのリスト`` =
  type s = { Name: string; Age: int }
  type t = { Person: s; Communities: string list }
  [<Scenario>]
  let OK() =
    Given "- { Person: { Name: hoge piyo, Age: 20 }, Communities: ['F#', Scala] }\n\
           - { Person: { Name: foo bar, Age: 30 }, Communities: [Java, 'C#'] }"
    |> When Yaml.load<t list>
    |> It should equal [ { Person = { Name = "hoge piyo"; Age = 20 }; Communities = ["F#"; "Scala"] }
                         { Person = { Name = "foo bar"; Age = 30 }; Communities = ["Java"; "C#"] } ]
    |> Verify

module ブロックのブロック =
  [<Scenario>]
  let OK() =
    Given ("- - 'F#'\n" +
          "  - Scala\n" +
          "- - Java\n" +
          "  - 'C#'")
    |> When Yaml.load<string list list>
    |> It should equal [ [ "F#"; "Scala" ]; [ "Java"; "C#"] ]
    |> Verify

module 複雑なYAML =
  type t = { a: string list; b: string list }
  [<Scenario>]
  let OK() =
    Given("- a:\n" +
          "  - a1\n" +
          "  - a2\n" +
          "  - a3\n" +
          "  b:\n" +
          "  - b1\n" +
          "  - b2\n" +
          "- b:\n" +
          "  - aa1\n" +
          "  - aa2\n" +
          "  a:\n" +
          "     \n" +
          "  \n" +
          "  - bb1")
    |> When Yaml.load<t list>
    |> It should equal [ { a = [ "a1"; "a2"; "a3" ]; b = [ "b1"; "b2" ] }
                         { b = [ "aa1"; "aa2" ]; a = [ "bb1" ] } ]
    |> Verify