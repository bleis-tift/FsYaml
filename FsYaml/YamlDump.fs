module YamlDump

open System.Text.RegularExpressions
open System.Text

open Microsoft.FSharp.Reflection

open Patterns

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
  |> Seq.map (fun info -> info.Name, info.GetValue(x, null))

let rec dumpBlockMap level (x: obj) =
  let result =
    x
    |> recordValues
    |> Seq.map (fun (name, value) ->
         new System.String(' ', level * 2) + name + ": " + (dump (level + 1) value)
    )
    |> Str.join "\n"
  if level = 0 then
    result
  else
    "\n" + result

and dump level (x: obj) =
  let t = x.GetType()
  match t with
  | PrimitiveType -> dumpPrimitive x
  | RecordType t -> dumpBlockMap level x
  | _ -> failwith "未実装なんですけど"
  