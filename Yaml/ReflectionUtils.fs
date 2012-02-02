module ReflectionUtils

open Microsoft.FSharp.Reflection
open Patterns

let elemType = function
| ListType ty -> ty
| otherwise -> failwith "%s is not list type." otherwise.Name

let convValue ty (x: obj) =
  let str2float = function
  | ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" -> infinity
  | "-.inf" | "-.Inf" | "-.INF" -> -infinity
  | ".nan" | ".NaN" | ".NAN" -> nan
  | otherwise -> float otherwise

  match ty with
  | IntType -> int (string x) |> box
  | FloatType -> str2float (string x) |> box
  | StrType -> string x |> box
  | OtherType ty -> x |> box

/// xs(obj list)をtのlistにする
/// (unboxするだけでは、obj listをint list等に変換できずに落ちる)
let specialize t (xs: obj list) =
  let nil, cons =
    let listType = typedefof<list<_>>.MakeGenericType([| t |])
    let cases = FSharpType.GetUnionCases(listType)
    cases.[0], cases.[1]
  // リフレクションを使ったcons
  let consR x xs =
    ref (FSharpValue.MakeUnion(cons, [| convValue t x; !xs |]))
  // リフレクションを使ったnil
  let nilR = FSharpValue.MakeUnion(nil, [||])
  // リストの移し替え
  !(List.foldBack consR xs (ref nilR))

/// プロパティ名と値のペアのリストを、ty型のレコードに変換する
let toRecord ty xs =
  let conv xs (field: System.Reflection.PropertyInfo) =
    xs
    |> List.find (fst >> ((=)field.Name))
    |> (snd >> (convValue field.PropertyType))
  
  if ty |> FSharpType.IsRecord then
    let args =
      ty |> FSharpType.GetRecordFields
         |> Array.map (conv xs)
    FSharpValue.MakeRecord(ty, args)
  else if ty |> FSharpType.IsUnion then
    null
  else if ty = typedefof<Map<_, _>> then
    null
  else
    failwith ""
