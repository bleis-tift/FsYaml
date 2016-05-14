module UtilityTest

open FsYaml.Utility
open Persimmon
open UseTestNameByReflection
open Assertions

module TypeTest =
  type GenericType<'a> = GeneticType of 'a

  let print =
    let body (t, expected) = test {
      do! should equal expected (Type.print t)
    }
    parameterize {
      case (typeof<int>, "int")
      case (typeof<System.DateTime>, "DateTime")
      case (typeof<int * string>, "int * string")
      case (typeof<int * (int * int)>, "int * (int * int)")
      case (typeof<int list>, "int list")
      case (typeof<int[] list>, "int[] list")
      case (typeof<(int * int) list>, "(int * int) list")
      case (typeof<int * int list>, "int * int list")
      case (typeof<Map<string, int>>, "Map<string, int>")
      case (typeof<GenericType<int>>, "GenericType<int>")
      case (typeof<int[]>, "int[]")
      run body
    }

module Attribute =
  open System

  type TestAttribute() = inherit Attribute()

  [<TestAttribute>]
  type WithAttribute() = class end

  type WithoutAttribute() = class end

  let tryGetCustomAttribute =
    let body (t, expected) = test {
      let actual = Attribute.tryGetCustomAttribute<TestAttribute> t
      do! Option.isSome actual |> should equal expected
    }
    parameterize {
      case (typeof<WithAttribute>, true)
      case (typeof<WithoutAttribute>, false)
      run body
    }

module PropertyInfoTest =
  open Microsoft.FSharp.Reflection

  type TestRecord = { Field: int }

  let ``{Type}.{Field}の形式でprintされる`` = test {
    let field = FSharpType.GetRecordFields(typeof<TestRecord>).[0]
    do! PropertyInfo.print field |> should equal "TestRecord.Field"
  }

module UnionTest =
  open Microsoft.FSharp.Reflection

  type TestUnion = Case

  let ``{Type}.{Case}の形式でprintされる`` = test {
    let unionCase = FSharpType.GetUnionCases(typeof<TestUnion>).[0]
    do! Union.printCase unionCase |> should equal "TestUnion.Case"
  }

module ObjectElementSeq =
  let ``obj seqをint seqにキャストできる`` = test {
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.cast typeof<int> xs
    do! unbox<int seq> actual |> should equalSeq (seq { 1..3 })
  }

  let ``空のseqをint seqにキャストできる`` = test {
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.cast typeof<int> xs
    do! unbox<int seq> actual |> should equalSeq (Seq.empty<int>)
  }

  let ``obj seqをint listに変換できる`` = test {
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.toList typeof<int> xs
    do! unbox<int list> actual |> should equal [ 1..3 ]
  }

  let ``空のseqをint listに変換できる`` = test {
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.toList typeof<int> xs
    do! unbox<int list> actual |> should equal ([]: int list)
  }

  let ``(obj * obj) seqをMap<string, int>に変換できる`` = test {
    let xs = [ ("1", 2); ("3", 4); ("4", 5) ] |> Seq.map (fun (k, v) -> (box k, box v))
    let actual = ObjectElementSeq.toMap typeof<string> typeof<int> xs
    let expected = Map.ofList [ ("1", 2); ("3", 4); ("4", 5) ]
    do! unbox<Map<string, int>> actual |> should equal expected
  }

  let ``空のseqをMap<string, int>に変換できる`` = test {
    let xs = Seq.empty<obj * obj>
    let actual = ObjectElementSeq.toMap typeof<string> typeof<int> xs
    do! unbox<Map<string, int>> actual |> should equal Map.empty<string, int>
  }

  let ``obj seqをint[]に変換できる`` = test {
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.toArray typeof<int> xs
    do! unbox<int[]> actual |> should equal [| 1..3 |]
  }

  let ``空のseqをint[]に変換できる`` = test {
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.toArray typeof<int> xs
    do! unbox<int[]> actual |> should equal (Array.empty<int>)
  }

module BoxedSeqTest =
  let ``listに対してmapできる`` = test {
    let xs = [ 1; 2; 3 ] |> box
    let actual = (typeof<int list>, xs) ||> RuntimeSeq.map (fun x -> string x)
    do! actual |> should equalSeq (Seq.ofList [ "1"; "2"; "3" ])
  }

  let ``arrayに対してmapできる`` = test {
    let xs = [| 1; 2; 3 |] |> box
    let actual = (typeof<int[]>, xs) ||> RuntimeSeq.map (fun x -> string x)
    do! actual |> should equalSeq (Seq.ofList [ "1"; "2"; "3" ])
  }

  let ``seqに対してmapできる`` = test {
    let xs = seq { 1..3 } |> box
    let actual = (typeof<int seq>, xs) ||> RuntimeSeq.map (fun x -> string x)
    do! actual |> should equalSeq (Seq.ofList [ "1"; "2"; "3" ])
  }

module BoxedMapTest =
  let ``mapをtoSeqできる`` = test {
    let map = Map.ofList [ ("a", 1); ("b", 2) ]
    let actual = RuntimeMap.toSeq typeof<Map<string, int>> map
    do! actual |> should equalSeq [ (box "a", box 1); (box "b", box 2) ]
  }

module SeqTest =
  let maxListByTest = 
    let body (xs, proj, expected) = test {
      let actual = xs |> Seq.maxListBy proj
      do! actual |> should equalSeq expected
    }
    parameterize {
      case ([], id, Seq.empty)
      case ([1; 2; 1; 0], (~-), seq [0])
      case ([1; 2; 1; 3], (fun x -> x / 2), seq [2; 3])
      run body
    }

module FuzzyTest =
  open FsYaml.Utility

  let flattenTest = test {
    let actual = [Fuzzy.create 0.2 1; Fuzzy.create 0.3 2; Fuzzy.create 0.4 3] |> Fuzzy.flatten
    let expected = Fuzzy.create (0.2 * 0.3 * 0.4) [1; 2; 3]
    do! actual |> should equal expected
  }
