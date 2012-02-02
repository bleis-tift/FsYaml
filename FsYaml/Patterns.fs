module Patterns

open Microsoft.FSharp.Reflection

type StrLitType = Quoted of string | Raw of string

let (|PrimitiveType|OptionType|ListType|RecordType|) t =
  let isPrimitive t =
    [ typeof<int>; typeof<float>; typeof<decimal>; typeof<string>; typeof<bool>; ]
    |> List.exists ((=)t)

  if isPrimitive t then
    PrimitiveType
  else if FSharpType.IsRecord t then
    RecordType t
  else if t.GetGenericTypeDefinition() = typedefof<option<_>> then
    OptionType (t.GetGenericArguments().[0])
  else if t.GetGenericTypeDefinition() = typedefof<list<_>> then
    ListType (t.GetGenericArguments().[0])
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
