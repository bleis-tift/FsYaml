module YamlExamples

open NUnit.Framework
open NaturalSpec

module OfficialTopPage =
  type s = { Ruby: string list }
  type t = { Projects: s }

  [<Scenario>]
  let test() =
    Given("Projects:\n" +
          "  Ruby:\n" +
          "  - psych         # libyaml wrapper (in Ruby core for 1.9.2)\n" +
          "  - RbYaml        # YAML 1.1 (PyYaml Port)\n" +
          "  - yaml4r        # YAML 1.0, standard library syck binding")
    |> When Yaml.load<t>
    |> It should equal { Projects = { Ruby = [ "psych"; "RbYaml"; "yaml4r" ] } }
    |> Verify

module ``Example 2-4`` =
  type t = { name: string; hr: int; avg: double }

  [<Scenario>]
  let test() =
    Given("-\n" +
          "  name: Mark McGwire\n" +
          "  hr:   65\n" +
          "  avg:  0.278\n" +
          "-\n" +
          "  name: Sammy Sosa\n" +
          "  hr:   63\n" +
          "  avg:  0.288")
    |> When Yaml.load<t list>
    |> It should equal [ { name = "Mark McGwire"; hr = 65; avg = 0.278 }
                         { name = "Sammy Sosa"; hr = 63; avg = 0.288 } ]
    |> Verify

module ``Example 2-17`` =
  type t = { unicode: string; control: string; hexesc: string; single: string; quoted: string; ``tie-fighter``:string }
  [<Scenario>]
  let test() =
    Given(@"unicode: ""Sosa did fine.\u263A""" + "\n" +
          @"control: ""\b1998\t1999\t2000\n""" + "\n" +
          @"hexesc:  ""\x0d\x0a is \r\n""" + "\n" +
          "\n" +
          @"single: '""Howdy!"" he cried.'" + "\n" +
          @"quoted: ' # not a ''comment''.'" + "\n" +
          @"tie-fighter: '|\-*-/|'")
    |> When Yaml.load<t>
    |> It should equal
       {
         unicode = "Sosa did fine.\u263A"
         control = "\b1998\t1999\t2000\n"
         hexesc  = "\r\n is \r\n"
         single  = "\"Howdy!\" he cried."
         quoted  = " # not a 'comment'."
         ``tie-fighter`` = @"|\-*-/|"
       }
    |> Verify