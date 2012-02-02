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
