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
module ObjectElementSeq =
  [<Test>]
  let ``obj seqをint seqにキャストできる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.cast typeof<int> xs
    unbox<int seq> actual |> should equal (seq { 1..3 })

  [<Test>]
  let ``空のseqをint seqにキャストできる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.cast typeof<int> xs
    unbox<int seq> actual |> should equal (Seq.empty<int>)

  [<Test>]
  let ``obj seqをint listに変換できる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.toList typeof<int> xs
    unbox<int list> actual |> should equal [ 1..3 ]

  [<Test>]
  let ``空のseqをint listに変換できる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.toList typeof<int> xs
    unbox<int list> actual |> should equal ([]: int list)

  [<Test>]
  let ``(obj * obj) seqをMap<string, int>に変換できる``() =
    let xs = [ ("1", 2); ("3", 4); ("4", 5) ] |> Seq.map (fun (k, v) -> (box k, box v))
    let actual = ObjectElementSeq.toMap typeof<string> typeof<int> xs
    let expected = Map.ofList [ ("1", 2); ("3", 4); ("4", 5) ]
    unbox<Map<string, int>> actual |> should equal expected

  [<Test>]
  let ``空のseqをMap<string, int>に変換できる``() =
    let xs = Seq.empty<obj * obj>
    let actual = ObjectElementSeq.toMap typeof<string> typeof<int> xs
    unbox<Map<string, int>> actual |> should equal Map.empty<string, int>

  [<Test>]
  let ``obj seqをint[]に変換できる``() =
    let xs = seq { 1..3 } |> Seq.map box
    let actual = ObjectElementSeq.toArray typeof<int> xs
    unbox<int[]> actual |> should equal [| 1..3 |]

  [<Test>]
  let ``空のseqをint[]に変換できる``() =
    let xs = Seq.empty<obj>
    let actual = ObjectElementSeq.toArray typeof<int> xs
    unbox<int[]> actual |> should equal (Array.empty<int>)

[<TestFixture>]
module BoxedSeqTest =
  [<Test>]
  let ``listに対してmapできる``() =
    let xs = [ 1; 2; 3 ] |> box
    let actual = (typeof<int list>, xs) ||> BoxedSeq.map (fun x -> string x)
    actual |> should equal (Seq.ofList [ "1"; "2"; "3" ])

  [<Test>]
  let ``arrayに対してmapできる``() =
    let xs = [| 1; 2; 3 |] |> box
    let actual = (typeof<int[]>, xs) ||> BoxedSeq.map (fun x -> string x)
    actual |> should equal (Seq.ofList [ "1"; "2"; "3" ])

  [<Test>]
  let ``seqに対してmapできる``() =
    let xs = seq { 1..3 } |> box
    let actual = (typeof<int seq>, xs) ||> BoxedSeq.map (fun x -> string x)
    actual |> should equal (Seq.ofList [ "1"; "2"; "3" ])

[<TestFixture>]
module BoxedMapTest =
  [<Test>]
  let ``mapをtoSeqできる``() =
    let map = Map.ofList [ ("a", 1); ("b", 2) ]
    let actual = BoxedMap.toSeq typeof<Map<string, int>> map
    actual |> should equal [ (box "a", box 1); (box "b", box 2) ]