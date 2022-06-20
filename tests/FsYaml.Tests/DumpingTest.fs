module DumpingTest

open System
open Expecto

open FsYaml
open FsYaml.RepresentationTypes

module Types=
  type Record = { FieldA: int; FieldB: string }
  

module Helper =
  let represent<'a> value = Native.represent<'a> TypeDefinitions.defaultDefinitions value

  let scalar x = Scalar (x, None)
  let plain x = scalar (Plain x)
  let nonPlain x = scalar (NonPlain x)
  let sequence x = Sequence (x, None)
  let mapping x = Mapping (Map.ofSeq x, None)
  let null' = Null None


open Types
open Helper

[<Tests>]
let tests =
 
  testList "Dumping tests" [

    testCase "can convert int" <| fun () ->
     "Must be equal" |> Expect.equal (represent 1) (plain "1")
    
    testCase "can convert int64" <| fun () ->
     "Must be equal" |> Expect.equal (represent 20L) (plain "20")
    
    testCase "can convert float - NaN" <| fun () ->
      "Must be equal" |> Expect.equal (represent Double.NaN) (plain ".NaN")
   
    testCase "can convert float - +.inf" <| fun () ->
      "Must be equal" |> Expect.equal (represent Double.PositiveInfinity) (plain "+.inf")
    
    testCase "can convert float - -.inf" <| fun () ->
      "Must be equal" |> Expect.equal (represent Double.NegativeInfinity) (plain "-.inf")
      
    testCase "can convert float - 45.5" <| fun () ->
      "Must be equal" |> Expect.equal (represent 45.5) (plain "45.5") 
  
    testCase "can convert string" <| fun () ->
      "Must be equal" |> Expect.equal (represent "this is text") (nonPlain "this is text") 
  
    testCase "can convert null string" <| fun () ->
      "Must be equal" |> Expect.equal (represent (null: string)) null'
  
    testCase "can convert bool - false" <| fun () ->
      "Must be equal" |> Expect.equal (represent false) (plain "false")
      
    testCase "can convert bool - true" <| fun () ->
      "Must be equal" |> Expect.equal (represent true) (plain "true")
      
    testCase "can convert decimal" <| fun () ->
      "Must be equal" |> Expect.equal (represent 30.5m) (plain "30.5")
      
    testCase "can convert DateTime" <| fun () ->
      "Must be equal" |> Expect.equal (represent (DateTime.Parse("2015-02-12 11:22:33.111"))) (nonPlain "2015-02-12 11:22:33.111")
      
    testCase "can convert TimeSpan" <| fun () ->
      "Must be equal" |> Expect.equal (represent (TimeSpan.Parse("11:22:33.444"))) (nonPlain "11:22:33.444")
      
    testCase "can convert record" <| fun () ->
      "Must be equal" |> Expect.equal (represent { FieldA = 20; FieldB = "abc" }) (mapping [ (plain "FieldA", plain "20"); (plain "FieldB", nonPlain "abc") ])
      
    testCase "can convert tuple" <| fun () ->
      "Must be equal" |> Expect.equal (represent (1, 3, "a")) (sequence [ (plain "1"); (plain "3"); (nonPlain "a") ])
      
    testCase "can convert list" <| fun () ->
      "Must be equal" |> Expect.equal (represent [ "a"; "b"; "c" ]) (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c")])
      
    testCase "can convert array" <| fun () ->
      "Must be equal" |> Expect.equal (represent [| "a"; "b"; "c" |]) (sequence [ (nonPlain "a"); (nonPlain "b"); (nonPlain "c")])
  
    testCase "can convert seq" <| fun () ->
      "Must be equal" |> Expect.equal (represent (seq { 1..3 })) (sequence [ (plain "1"); (plain "2"); (plain "3")])
      
    testCase "can convert set" <| fun () ->
      "Must be equal" |> Expect.equal (represent (set [ 1; 2; 3 ])) (sequence [ (plain "1"); (plain "2"); (plain "3")])

    testCase "can convert map" <| fun () ->
      "Must be equal" |> Expect.equal (represent (Map.ofList [ ("a", 1); ("b", 2) ])) (mapping [ (nonPlain "a", plain "1"); (nonPlain "b", plain "2") ])
      
    testCase "can convert Option.Some" <| fun () ->
     "Must be equal" |> Expect.equal (represent (Some "abc")) (nonPlain "abc")
     
    testCase "can convert Option.None" <| fun () ->
     "Must be equal" |> Expect.equal (represent (None: int option)) null'
  ]
(*
  
module DumpRecordTest =
  open System

  let representOmittingDefaultFields<'a> value = Native.represent<'a> (TypeDefinitions.recordDefOmittingDefaultFields :: TypeDefinitions.defaultDefinitions) value

  type TestRecord =
    { FieldA: int; FieldB: option<int> }
  with
    static member DefaultFieldA = -1

  let ``デフォルトでは省略可能なフィールドも出力される`` = test {
    let actual = represent { FieldA = -1; FieldB = None }
    do! actual |> should equal (mapping [(plain "FieldA", plain "-1"); (plain "FieldB", null')])
  }

  let ``省略可能なフィールドを出力しない定義が機能する`` = test {
      let actual = representOmittingDefaultFields { FieldA = -1; FieldB = None }
      do! actual |> should equal (mapping [ ])
    }

module DumpUnionTest =
  type TestUnion =
    | NoValue
    | OneField of int
    | ManyFields of int * int
    | OneNamedField of fieldA: int
    | NamedFields of fieldA: int * fieldB: string

  let ``値がないケースを変換できる`` = test {
    let actual = represent NoValue
    do! actual |> should equal (plain "NoValue")
  }

  let ``値が1つのケースを変換できる`` = test {
    let actual = represent (OneField 23)
    do! actual |> should equal (mapping [ (plain "OneField", plain "23") ])
  }

  let ``値が複数のケースを変換できる`` = test {
    let actual = represent (ManyFields (1, 3))
    do! actual |> should equal (mapping [ (plain "ManyFields", sequence [ (plain "1"); (plain "3") ])])
  }

  let ``値が1つの名前付きフィールドを変換できる`` = test {
    let actual = represent (OneNamedField (fieldA = 1))
    do! actual |> should equal (mapping [ (plain "OneNamedField", mapping [ (plain "fieldA", plain "1") ]) ])
  }

  let ``値が複数の名前付きフィールドを変換できる`` = test {
    let actual = represent (NamedFields (fieldA = 3, fieldB = "abc"))
    do! actual |> should equal (mapping [ (plain "NamedFields", mapping [ (plain "fieldA", plain "3"); (plain "fieldB", nonPlain "abc") ]) ])
  }
*)