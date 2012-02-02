module Patterns

open Microsoft.FSharp.Reflection

type StrLitType = Quoted of string | Raw of string

let (|PrimitiveType|OptionType|ListType|RecordType|) t =
  if t = typeof<int> then
    PrimitiveType
  else if t = typeof<string> then
    PrimitiveType
  else if t = typeof<double> then
    PrimitiveType
  else if FSharpType.IsRecord t then
    RecordType t
  else if t.GetGenericTypeDefinition() = typedefof<option<_>> then
    OptionType (t.GetGenericArguments().[0])
  else if t.GetGenericTypeDefinition() = typedefof<list<_>> then
    ListType (t.GetGenericArguments().[0])
  else
    failwithf "%s is not supported type." t.Name

let (|IntType|FloatType|StrType|OtherType|Opt|) t =
  if t = typeof<int> then IntType
  else if t = typeof<float> then FloatType
  else if t = typeof<string> then StrType
  else if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>> then Opt(t.GetGenericArguments().[0])
  else OtherType t
