module Patterns

open Microsoft.FSharp.Reflection

let (|PrimitiveType|ListType|RecordType|) t =
  if t = typeof<int> then
    PrimitiveType
  else if t = typeof<string> then
    PrimitiveType
  else if t = typeof<double> then
    PrimitiveType
  else if FSharpType.IsRecord t then
    RecordType t
  else if t.GetGenericTypeDefinition() = typedefof<list<_>> then
    ListType (t.GetGenericArguments().[0])
  else
    failwithf "%s is not supported type." t.Name

let (|IntType|StrType|DoubleType|OtherType|) t =
  if t = typeof<int> then IntType
  else if t = typeof<string> then StrType
  else if t = typeof<double> then DoubleType
  else OtherType t
