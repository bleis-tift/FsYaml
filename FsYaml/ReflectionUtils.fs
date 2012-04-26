module ReflectionUtils

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Patterns

let elemType = function
| ListType ty -> ty
| otherwise -> failwith "%s is not list type." otherwise.Name

let convValue (ty: System.Type) (x: obj) =
  let str x =
    match unbox x with
    | Raw("~" | "null" | "Null" | "NULL" | "") -> failwithf "this type(%s) does not allow null value." (ty.Name)
    | Raw s | Quoted s -> s

  let str2float = function
  | ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" -> infinity
  | "-.inf" | "-.Inf" | "-.INF" -> -infinity
  | ".nan" | ".NaN" | ".NAN" -> nan
  | otherwise -> float otherwise

  let str2bool = function
  | "y" | "Y" | "yes" | "Yes" | "YES" | "true" | "True" | "TRUE" | "on" | "On" | "ON" -> true
  | "n" | "N" | "no" | "No" | "NO" | "false" | "False" | "FALSE" | "off" | "Off" | "OFF" -> false
  | otherwise -> bool.Parse otherwise

  let str2option f x =
    match unbox x with
    | Raw("~" | "null" | "Null" | "NULL" | "") -> None
    | Quoted s | Raw s -> Some (f s)

  match ty with
  | IntType -> int (str x) |> box
  | FloatType -> str2float (str x) |> box
  | DecimalType -> decimal (str x) |> box
  | StrType -> str x |> box
  | BoolType -> str2bool (str x) |> box
  | Opt IntType -> str2option int x |> box
  | Opt FloatType -> str2option float x |> box
  | Opt DecimalType -> str2option decimal x |> box
  | Opt StrType -> str2option string x |> box
  | Opt ty -> failwithf "%s is not supported type." ty.Name
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

let toMap keyType valType xs =
  let kvType = FSharpType.MakeTupleType [| keyType; valType |]
  let mapType = typedefof<Map<_, _>>.MakeGenericType([| keyType; valType |])
  let makeTuple (k, v) =
    FSharpValue.MakeTuple ([| convValue keyType k; convValue valType v |], kvType)
  let xs =
    xs
    |> List.map (makeTuple)
    |> specialize kvType
  mapType.GetConstructors().[0].Invoke([| xs |])

let asm = typedefof<Map<_, _>>.Assembly

let normalizeMap (x: obj): Map<IComparable, obj> =
  let keyType, valueType =
    let genArgs = x.GetType().GetGenericArguments()
    genArgs.[0], genArgs.[1]
  let kvType = FSharpType.MakeTupleType([| keyType; valueType |])
  
  let mapModule = asm.GetType("Microsoft.FSharp.Collections.MapModule")
  let toList = mapModule.GetMethod("ToList").MakeGenericMethod(keyType, valueType)
  let listed = toList.Invoke(null, [| x |])

  let listModule = asm.GetType("Microsoft.FSharp.Collections.ListModule")
  let iter = listModule.GetMethod("Iterate").MakeGenericMethod(kvType)
  let iterFunType = FSharpType.MakeFunctionType(kvType, typedefof<unit>)
  let result = ref Map.empty
  let iterFunImpl x =
    let values = FSharpValue.GetTupleFields(x)
    result := !result |> Map.add (values.[0] |> unbox) (values.[1])
    box ()
  let iterFun = FSharpValue.MakeFunction(iterFunType, iterFunImpl)
  iter.Invoke(null, [| iterFun; listed |]) |> ignore
  
  !result

let normalizeList (x: obj) : obj list =
  let valueType = x.GetType().GetGenericArguments().[0]

  let listModule = asm.GetType("Microsoft.FSharp.Collections.ListModule")
  let iter = listModule.GetMethod("Iterate").MakeGenericMethod(valueType)
  let iterFunType = FSharpType.MakeFunctionType(valueType, typedefof<unit>)
  let result = ref []
  let iterFunImpl x =
    result := (box x) :: !result
    box ()
  let iterFun = FSharpValue.MakeFunction(iterFunType, iterFunImpl)
  iter.Invoke(null, [| iterFun; x |]) |> ignore

  List.rev !result

let normalizeOption (x: obj) : obj option =
  let valueType = x.GetType().GetGenericArguments().[0]
  let optionModule = asm.GetType("Microsoft.FSharp.Core.OptionModule")
  let isSome = optionModule.GetMethod("IsSome").MakeGenericMethod([| valueType |])
  let get = optionModule.GetMethod("GetValue").MakeGenericMethod([| valueType |])
  
  if (isSome.Invoke(null, [| x |]) :?> bool) then
    let obj = get.Invoke(null, [| x |])
    Some obj
  else
    None

/// ケース識別子と値のペアを、ty型の判別共用体に変換する
let toUnion ty xs =
  match xs with
  | [(case, value)] ->
      let c = FSharpType.GetUnionCases(ty) |> Array.find (fun c -> c.Name = case)
      FSharpValue.MakeUnion(c, [| value |])
  | _ -> failwith "oops!"

let defaultValue typ name =
  let prop = (typ: Type).GetProperty("Default" + name)
  Raw (string <| prop.GetValue(null, null))

/// プロパティ名と値のペアのリストを、ty型のレコードに変換する
let toRecord ty xs =
  let conv xs (field: System.Reflection.PropertyInfo) =
    xs
    |> List.tryFind (fst >> ((=)field.Name))
    |> function
       | Some (_, x) -> (convValue field.PropertyType x)
       | _ -> convValue field.PropertyType (defaultValue ty field.Name)
  
  let args =
    ty |> FSharpType.GetRecordFields
       |> Array.map (conv xs)
  FSharpValue.MakeRecord(ty, args)
