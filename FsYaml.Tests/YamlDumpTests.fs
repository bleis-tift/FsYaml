module YamlDumpTests

open NUnit.Framework
open NaturalSpec

[<TestFixture>]
module StringDump =
  let Cases =
    TestWith (doubleParam "あいうえお" "あいうえお")
    |> And (doubleParam "かきくけこ" "かきくけこ")
    |> And (doubleParam "かきくけ\"こ" "\"かきくけ\\\"こ\"")
    |> And (doubleParam "かきく\r\nけこ" "\"かきく\r\nけこ\"")
    |> And (doubleParam "かきく\bけこ" "\"かきく\bけこ\"")

  [<ScenarioSource "Cases">]
  let test (input: string) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

[<TestFixture>]
module IntDump =
  let Cases =
    TestWith (doubleParam 1 "1")
    |> And (doubleParam 0 "0")
    |> And (doubleParam -56 "-56")
    |> And (doubleParam 3000 "3000")

  [<ScenarioSource "Cases">]
  let test (input: int) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

[<TestFixture>]
module FloatDump =
  let Cases =
    TestWith (doubleParam 1.5 "1.5")
    |> And (doubleParam 30.5 "30.5")
    |> And (doubleParam 0 "0")
    |> And (doubleParam -2.5 "-2.5")
    |> And (doubleParam infinity ".inf")
    |> And (doubleParam -infinity "-.inf")
    |> And (doubleParam nan ".nan")

  [<ScenarioSource "Cases">]
  let test (input: float) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify

[<TestFixture>]
module DecimalDump =
  let Cases =
    TestWith (doubleParam 1.5m "1.5")
    |> And (doubleParam 30.6m "30.6")
    |> And (doubleParam 0m "0")
    |> And (doubleParam -2.8m "-2.8")

  [<ScenarioSource "Cases">]
  let test (input: decimal) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify