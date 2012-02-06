module YamlDumpTests

open NUnit.Framework
open NaturalSpec

[<TestFixture>]
module StringDump =
  let DumpCases =
    TestWith (doubleParam "あいうえお" "あいうえお")
    |> And (doubleParam "かきくけこ" "かきくけこ")
    |> And (doubleParam "かきくけ\"こ" "\"かきくけ\\\"こ\"")
    |> And (doubleParam "かきく\r\nけこ" "\"かきく\r\nけこ\"")
    |> And (doubleParam "かきく\bけこ" "\"かきく\bけこ\"")

  [<ScenarioSource "DumpCases">]
  let test (input: string) (expected: string) =
    Given input
    |> When Yaml.dump
    |> It should equal expected
    |> Verify
