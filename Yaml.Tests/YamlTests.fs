module YamlTests

open NUnit
open NaturalSpec

module ScalarTypes =
  let FloatCases =
    TestWith (doubleParam ".inf" infinity)
    |> And (doubleParam ".Inf" infinity)
    |> And (doubleParam ".INF" infinity)
    |> And (doubleParam "+.inf" infinity)
    |> And (doubleParam "+.Inf" infinity)
    |> And (doubleParam "+.INF" infinity)
    |> And (doubleParam "-.inf" -infinity)
    |> And (doubleParam "-.Inf" -infinity)
    |> And (doubleParam "-.INF" -infinity)
    |> And (doubleParam "0" 0.0)
    |> And (doubleParam "0.5" 0.5)
    |> And (doubleParam "-0.5" -0.5)
    |> And (doubleParam "-0.5e-0" -0.5e-0)
  [<ScenarioSource "FloatCases">]
  let floatの解析(yaml: string, expected: float) =
    Given yaml
    |> When Yaml.load<float>
    |> It should equal expected
    |> Verify

  let nan x = System.Double.IsNaN(x)

  [<Example ".nan">]
  [<Example ".NaN">]
  [<Example ".NAN">]
  let nanの解析(yaml: string) =
    Given yaml
    |> When Yaml.load<float>
    |> It should be nan
    |> Verify

  [<Example "~">]
  [<Example "null">]
  [<Example "Null">]
  [<Example "NULL">]
  [<Example "">]
  let nullの解析(yaml: string) =
    Given yaml
    |> When Yaml.load<string option>
    |> It should equal None
    |> Verify

  [<Example("'~'", "~")>]
  [<Example("\"~\"", "~")>]
  [<Example("'null'", "null")>]
  [<Example("\"null\"", "null")>]
  [<Example("''", "")>]
  [<Example("\"\"", "")>]
  let nullに解析されない yaml expected =
    Given yaml
    |> When Yaml.load<string option>
    |> It should equal (Some expected)
    |> Verify

  [<Example("y", true)>]
  [<Example("Y", true)>]
  [<Example("yes", true)>]
  [<Example("Yes", true)>]
  [<Example("YES", true)>]
  [<Example("n", false)>]
  [<Example("N", false)>]
  [<Example("no", false)>]
  [<Example("No", false)>]
  [<Example("NO", false)>]
  [<Example("true", true)>]
  [<Example("True", true)>]
  [<Example("TRUE", true)>]
  [<Example("false", false)>]
  [<Example("False", false)>]
  [<Example("FALSE", false)>]
  [<Example("on", true)>]
  [<Example("On", true)>]
  [<Example("ON", true)>]
  [<Example("off", false)>]
  [<Example("Off", false)>]
  [<Example("OFF", false)>]
  let boolの解析 yaml expected =
    Given yaml
    |> When Yaml.load<bool>
    |> It should equal expected
    |> Verify