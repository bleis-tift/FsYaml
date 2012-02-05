module Patterns

open Microsoft.FSharp.Reflection

type StrLitType = Quoted of string | Raw of string

let (|PrimitiveType|OptionType|ListType|MapType|UnionType|RecordType|) t =
  let isPrimitive t =
    [ typeof<int>; typeof<float>; typeof<decimal>; typeof<string>; typeof<bool>; ]
    |> List.exists ((=)t)

  if isPrimitive t then
    PrimitiveType
  else if FSharpType.IsRecord t then
    RecordType t
  else if t.IsGenericType then
    let gent = t.GetGenericTypeDefinition()
    let args = t.GetGenericArguments()
    if gent = typedefof<option<_>> then
      OptionType args.[0]
    else if gent = typedefof<list<_>> then
      ListType args.[0]
    else if gent = typedefof<Map<_, _>> then
      MapType (args.[0], args.[1])
    else
      failwithf "%s is not supported type." t.Name
  else if FSharpType.IsUnion t then
    UnionType t
  else
    failwithf "%s is not supported type." t.Name

let (|IntType|FloatType|DecimalType|StrType|BoolType|OtherType|Opt|) t =
  if t = typeof<int> then IntType
  else if t = typeof<float> then FloatType
  else if t = typeof<decimal> then DecimalType
  else if t = typeof<string> then StrType
  else if t = typeof<bool> then BoolType
  else if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then Opt(t.GetGenericArguments().[0])
  else OtherType t
