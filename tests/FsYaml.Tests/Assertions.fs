module Assertions

open Persimmon
open System
open System.Linq

let equalSeq (expected: seq<'a>) (actual: seq<'a>) =
  if Enumerable.SequenceEqual(actual, expected) then pass ()
  else fail (sprintf "Expect: %A\nActual: %A" expected actual)

let equalFloat (expected: float) (actual: float) =
  if Double.IsNaN(expected) && Double.IsNaN(actual) then pass()
  else assertEquals expected actual

let should = id
let equal = assertEquals