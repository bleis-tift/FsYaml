module UtilityTest

open NUnit.Framework
open FsUnit

open FsYaml.Utility

[<TestFixture>]
module TypeTest =
  type GenericType<'a> = GeneticType of 'a

  [<TestCase(typeof<int>, "int")>]
  [<TestCase(typeof<System.DateTime>, "DateTime")>]
  [<TestCase(typeof<int * string>, "int * string")>]
  [<TestCase(typeof<int * (int * int)>, "int * (int * int)")>]
  [<TestCase(typeof<int list>, "int list")>]
  [<TestCase(typeof<int[] list>, "int[] list")>]
  [<TestCase(typeof<(int * int) list>, "(int * int) list")>]
  [<TestCase(typeof<int * int list>, "int * int list")>]
  [<TestCase(typeof<Map<string, int>>, "Map<string, int>")>]
  [<TestCase(typeof<GenericType<int>>, "GenericType<int>")>]
  [<TestCase(typeof<int[]>, "int[]")>]
  let ``print`` t (expected: string) = Type.print t |> should equal expected
  
[<TestFixture>]
module ObjectSeqTest =
  [<Test>]
  let ``obj seqをint seqにキャストできる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectSeq.cast typeof<int> xs
    unbox<int seq> actual |> should equal (seq { 1..3 })

  [<Test>]
  let ``空のseqをint seqにキャストできる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectSeq.cast typeof<int> xs
    unbox<int seq> actual |> should equal (Seq.empty<int>)

  [<Test>]
  let ``obj seqをint listに変換できる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectSeq.toList typeof<int> xs
    unbox<int list> actual |> should equal [ 1..3 ]

  [<Test>]
  let ``空のseqをint listに変換できる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectSeq.toList typeof<int> xs
    unbox<int list> actual |> should equal ([]: int list)

  [<Test>]
  let ``(obj * obj) seqをMap<string, int>に変換できる``() =
    let xs = [ ("1", 2); ("3", 4); ("4", 5) ] |> Seq.map (fun (k, v) -> (box k, box v))
    let actual = ObjectSeq.toMap typeof<string> typeof<int> xs
    let expected = Map.ofList [ ("1", 2); ("3", 4); ("4", 5) ]
    unbox<Map<string, int>> actual |> should equal expected

  [<Test>]
  let ``空のseqをMap<string, int>に変換できる``() =
    let xs = Seq.empty<obj * obj>
    let actual = ObjectSeq.toMap typeof<string> typeof<int> xs
    unbox<Map<string, int>> actual |> should equal Map.empty<string, int>

  [<Test>]
  let ``obj seqをint[]に変換できる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectSeq.toArray typeof<int> xs
    unbox<int[]> actual |> should equal [| 1..3 |]

  [<Test>]
  let ``空のseqをint[]に変換できる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectSeq.toArray typeof<int> xs
    unbox<int[]> actual |> should equal (Array.empty<int>)