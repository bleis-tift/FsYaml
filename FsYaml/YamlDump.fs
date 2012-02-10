module YamlDump

open System
open System.Text.RegularExpressions
open System.Text

open Microsoft.FSharp.Reflection

open Patterns
open ReflectionUtils

let dumpString str =
  let pat = @"[\b\r\n\t""]"
  if(Regex.IsMatch(str, pat)) then
    let ret = new StringBuilder()
    ret.Append(@"""").Append(Regex.Replace(str, @"""", @"\""")).Append(@"""") |> ignore
    ret.ToString()
  else
    str

let dumpFloat f =
  let (|Infinity|NegativeInfinity|NaN|Float|)f =
    if System.Double.IsNaN f then NaN
    else if f = infinity then Infinity
    else if f = -infinity then NegativeInfinity
    else Float f

  match f with
  | Infinity -> ".inf"
  | NegativeInfinity -> "-.inf"
  | NaN -> ".nan"
  | Float f -> string f

let dumpPrimitive (x: obj) =
  let t = x.GetType()
  match t with
  | StrType -> dumpString (x :?> string)
  | IntType -> string x
  | FloatType -> dumpFloat (x :?> float)
  | DecimalType -> string x
  | BoolType -> x |> string |> Str.lower
  | _ -> failwith "未実装なんですけど"

let recordValues x =
  FSharpType.GetRecordFields (x.GetType())
  |> Seq.map (fun info -> info.Name :> IComparable, info.GetValue(x, null))
  |> List.ofSeq

let rec dumpBlockMap level values =
  let result =
    values
    |> List.map (fun (name, value) ->
         let name = string name
         let value = dump (level + 1) value
         new System.String(' ', level * 2) + name + ": " + value
    )
    |> Str.join "\n"
  if level = 0 then
    result
  else
    "\n" + result
and dumpInlineMap values =
  let items =
    values
    |> List.map (fun (name, value) -> (string name) + ": " + (dumpPrimitive value))
    |> Str.join ", "
  "{ " + items + " }"
and dumpMap level x =
  x.GetType().GetGenericArguments().[1]
  |> function
     | PrimitiveType ->
         x
         |> normalizeMap
         |> Map.toList
         |> dumpInlineMap
     | _ ->
         x
         |> normalizeMap
         |> Map.toList
         |> dumpBlockMap level
and dump level (x: obj) =
  let t = x.GetType()
  match t with
  | PrimitiveType -> dumpPrimitive x
  | RecordType t -> x |> recordValues |> dumpBlockMap level 
  | MapType t -> dumpMap level x
  | _ -> failwith "未実装なんですけど"
  