module YamlDumpTests

open NUnit.Framework
open NaturalSpec

[<TestFixture>]
module PrimitiveDump =
  let StringCases =
    TestWith (doubleParam "あいうえお" "あいうえお")
    |> And (doubleParam "かきくけこ" "かきくけこ")
    |> And (doubleParam "かきくけ\"こ" "\"かきくけ\\\"こ\"")
    |> And (doubleParam "かきく\r\nけこ" "\"かきく\r\nけこ\"")
    |> And (doubleParam "かきく\bけこ" "\"かきく\bけこ\"")

  [<ScenarioSource "StringCases">]
  let stringをdumpできる (input: string) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

  let IntCases =
    TestWith (doubleParam 1 "1")
    |> And (doubleParam 0 "0")
    |> And (doubleParam -56 "-56")
    |> And (doubleParam 3000 "3000")

  [<ScenarioSource "IntCases">]
  let intをdumpできる (input: int) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

  let FloatCases =
    TestWith (doubleParam 1.5 "1.5")
    |> And (doubleParam 30.5 "30.5")
    |> And (doubleParam 0 "0")
    |> And (doubleParam -2.5 "-2.5")
    |> And (doubleParam infinity ".inf")
    |> And (doubleParam -infinity "-.inf")
    |> And (doubleParam nan ".nan")

  [<ScenarioSource "FloatCases">]
  let floatをdumpできる (input: float) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

  let DecimalCases =
    TestWith (doubleParam 1.5m "1.5")
    |> And (doubleParam 30.6m "30.6")
    |> And (doubleParam 0m "0")
    |> And (doubleParam -2.8m "-2.8")

  [<ScenarioSource "DecimalCases">]
  let decimalをdumpできる (input: decimal) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

  let BoolCases =
    TestWith (doubleParam true "true")
    |> And (doubleParam false "false")

  [<ScenarioSource "BoolCases">]
  let boolをdumpできる (input: bool) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

[<TestFixture>]
module RecordDump =
  type t1 = { name: string; value: int }
  [<Scenario>]
  let 値がプリミティブなrecordをdumpできる() =
    let o = { name = "あいうえお"; value = 3 } : t1
    Given o
    |> When Yaml.dump
    |> It should equal ("name: あいうえお\n" +
                        "value: 3")
    |> Verify

  type t2 = { id: int; value: t1 }
  [<Scenario>]
  let 値にrecordを持つrecordをdumpできる() =
    let o = { id = 40; value = { name = "かきくけこ"; value = 90 } }
    Given o
    |> When Yaml.dump
    |> It should equal ("id: 40\n" +
                        "value: \n" +
                        "  name: かきくけこ\n" +
                        "  value: 90")
    |> Verify


  type t3 = { name: string; values: int list }
  [<Scenario>]
  let 値にlistを持つrecordをdumpできる() =
    Given { name = "aiueo"; values = [ 2; 3; ] }
    |> When Yaml.dump
    |> It should equal ("name: aiueo\n" +
                        "values: [ 2, 3 ]")
    |> Verify
[<TestFixture>]
module MapDump =
  [<Scenario>]
  let 値がプリミティブのmapをdumpできる() =
    Given (Map.ofList [ "one", 1; "two", 2; "three", 3; ])
    |> When Yaml.dump
    |> It should equal "{ one: 1, three: 3, two: 2 }" // key の昇順になる
    |> Verify
    
  type t = { name: string; value: int }
  let makeT name value = { name = name; value = value }
  [<Scenario>]
  let 値がプリミティブ以外のmapをdumpできる() =
    Given (Map.ofList [ "one", makeT "ONE" 1; "two", makeT "TWO" 2; ])
    |> When Yaml.dump
    |> It should equal ("one: \n" +
                        "  name: ONE\n" +
                        "  value: 1\n" +
                        "two: \n" +
                        "  name: TWO\n" +
                        "  value: 2")
    |> Verify

[<TestFixture>]
module ListDump =
  [<Scenario>]
  let 値がプリミティブなlistをdumpできる() =
    Given [ 2; 3; 4; 5 ]
    |> When Yaml.dump
    |> It should equal "[ 2, 3, 4, 5 ]"
    |> Verify

  type t = { name: string; value: int }
  let makeT name value = { name = name; value = value; }
  [<Scenario>]
  let 値がrecordなlistをdumpできる() =
    Given [
      makeT "a" 1
      makeT "b" 2
    ]
    |> When Yaml.dump
    |> It should equal ("- \n" +
                        "  name: a\n" +
                        "  value: 1\n" +
                        "- \n" +
                        "  name: b\n" +
                        "  value: 2")
    |> Verify

[<TestFixture>]
module OptionDump =
  type t = { value: string option }
  [<Scenario>]
  let Someをdumpできる() =
    Given { value = Some "hoge" }
    |> When Yaml.dump
    |> It should equal "value: hoge"
    |> Verify

  [<Scenario>]
  let Noneの場合はnull() =
    Given { value = None }
    |> When Yaml.dump
    |> It should equal "value: null"
    |> Verify

[<TestFixture>]
module UnionDump =
  type U =
    | A
    | B of string
    | C of int list
    | D of string * int

  [<Scenario>]
  let 項がない判別共用体をdumpできる() =
    Given A
    |> When Yaml.dump
    |> It should equal "A: "
    |> Verify

  [<Scenario>]
  let 項がstringの判別共用体をdumpできる() =
    Given (B "aaa")
    |> When Yaml.dump
    |> It should equal "B: aaa"
    |> Verify

  [<Scenario>]
  let 項がlistの判別共用体をdumpできる() =
    Given (C [ 1; 2; 3; ])
    |> When Yaml.dump
    |> It should equal "C: [ 1, 2, 3 ]"
    |> Verify

  [<Scenario()>]
  [<ExpectedException>]
  let ``2項以上の場合は例外が発生する``() =
    Given (D ("aiueo", 3))
    |> When Yaml.dump
    |> Verify
