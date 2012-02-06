module YamlDump

open System.Text.RegularExpressions
open System.Text

open Patterns

let dumpString str =
  let pat = @"[\b\r\n\t""]"
  if(Regex.IsMatch(str, pat)) then
    let ret = new StringBuilder()
    ret.Append(@"""").Append(Regex.Replace(str, @"""", @"\""")).Append(@"""") |> ignore
    ret.ToString()
  else
    str

let dumpInt i = string i

let dumpFloat f =
  let (|Infinity|NegativeInfinity|NaN|Float|)f =
    if System.Double.IsNaN f then NaN
    else if f = infinity then Infinity
    else if f = -infinity then NegativeInfinity
    else Float f

  let nInfinity = -infinity
  match f with
  | Infinity -> ".inf"
  | NegativeInfinity -> "-.inf"
  | NaN -> ".nan"
  | Float f -> string f

let dump (o: obj) =
  let t = o.GetType()
  match t with
  | StrType -> dumpString (o :?> string)
  | IntType -> string o
  | FloatType -> dumpFloat (o :?> float)
  | DecimalType -> string o
  | _ -> failwith "未実装なんですけど"
  