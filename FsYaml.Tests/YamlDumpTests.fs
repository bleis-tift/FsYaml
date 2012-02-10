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

[<TestFixture>]
module MapDump =
  [<Scenario>]
  let 値がプリミティブのmapをdumpできる() =
    Given (Map.ofList [ "one", 1; "two", 2; "three", 3; ])
    |> When Yaml.dump
    |> It should equal "{ one: 1, three: 3, two: 2 }" // key の昇順になる
    |> Verify
    