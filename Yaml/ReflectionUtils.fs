module ReflectionUtils

open Microsoft.FSharp.Reflection
open Patterns

/// xs(obj list)をtのlistにする
/// (unboxするだけでは、obj listをint list等に変換できずに落ちる)
let specialize t (xs: obj list) =
  let nil, cons =
    let listType = typedefof<list<_>>.MakeGenericType([| t |])
    let cases = FSharpType.GetUnionCases(listType)
    cases.[0], cases.[1]
  // リフレクションを使ったcons
  let consR x xs =
    ref (FSharpValue.MakeUnion(cons, [| x; !xs |]))
  // リフレクションを使ったnil
  let nilR = FSharpValue.MakeUnion(nil, [||])
  // リストの移し替え
  !(List.foldBack consR xs (ref nilR))

/// プロパティ名と値のペアのリストを、ty型のレコードに変換する
let toRecord ty xs =
  let convTo t (x: obj) =
    match t with
    | IntType -> int (string x) |> box
    | DoubleType -> double (string x) |> box
    | StrType | OtherType _ -> x
  let conv (field: System.Reflection.PropertyInfo) =
    xs
    |> List.find (fst >> ((=)field.Name))
    |> (snd >> (convTo field.PropertyType))
  
  if ty |> FSharpType.IsRecord then
    let args =
      ty |> FSharpType.GetRecordFields
         |> Array.map conv
    FSharpValue.MakeRecord(ty, args)
  else if ty |> FSharpType.IsUnion then
    null
  else if ty = typedefof<Map<_, _>> then
    null
  else
    failwith ""