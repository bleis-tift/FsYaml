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

module ``Map<string, string>`` =
  [<Scenario>]
  let ``{ aaa: bbb, ccc: ddd }というYAML文字列をmap ["aaa", "bbb"; "ccc", "ddd"]に変換できる``() =
    Given "{ aaa: bbb, ccc: ddd }"
    |> When Yaml.load<Map<string, string>>
    |> It should equal (Map.ofList ["aaa", "bbb"; "ccc", "ddd"])
    |> Verify

  [<Scenario>]
  let ブロック形式のマップも扱える() =
    Given "aaa: bbb\n\
           ccc: ddd"
    |> When Yaml.load<Map<string, string>>
    |> It should equal (Map.ofList ["aaa", "bbb"; "ccc", "ddd"])
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

module ``判別共用体`` =
  type LocalInfo = { Path: string }
  type RedmineInfo = { BaseUrl: string; ProjectId: string; AccessKey: string }
  type GithubInfo = { ProjectId: string }
  type TicketSource =
  | Local of LocalInfo
  | Redmine of RedmineInfo
  | Github of GithubInfo
  [<Scenario>]
  let OK() =
    Given("- Github: { ProjectId: bleis-tift/FsYaml }\n" +
          "- Redmine:\n" +
          "    BaseUrl: http://rdm/\n" +
          "    ProjectId: project001\n" +
          "    AccessKey: aaa")
    |> When Yaml.load<TicketSource list>
    |> It should equal [ Github { ProjectId = "bleis-tift/FsYaml" }; Redmine { BaseUrl = "http://rdm/"; ProjectId = "project001"; AccessKey = "aaa" } ]
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

module optionを含まない型にnullが来た場合 =
  type t = { Hoge: string }
  [<Example "Hoge: ">]
  [<Example "Hoge: ~">]
  [<Example "Hoge: null">]
  [<FailsWithType (typeof<System.Exception>) >]
  let stringにnullが来た場合 yaml =
    Given yaml
    |> When Yaml.load<t>
    |> Verify
