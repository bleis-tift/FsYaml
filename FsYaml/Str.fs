module Str

open System
open System.Text.RegularExpressions

let trim (s: string) = Patterns.Raw (s.Trim())

let replaceEscapeSequence str =
  let pat = @"(?:\\b|\\n|\\r|\\t|\\x([0-9a-fA-F]{2})|\\u([0-9a-fA-F]{4})|\\u([0-9a-fA-F]{8})|\\\\)" 
  Regex.Replace(str, pat, fun (m: Match) ->
    match m.Value, m.Length with
    | @"\b", _ -> "\b"
    | @"\n", _ -> "\n"
    | @"\r", _ -> "\r"
    | @"\t", _ -> "\t"
    | @"\\", _ -> "\\"
    | v, _ when v.StartsWith(@"\x") ->
      let num = Convert.ToInt32((m.Groups.Item 1).Value, 16)
      Char.ConvertFromUtf32(num)
    | _, n ->
      let idx = if n = @"\u0000".Length then 2 else 3
      let num = Convert.ToInt32((m.Groups.Item idx).Value, 16)
      Char.ConvertFromUtf32(num)
  )

let lower str = (str: string).ToLower()

let join sep (str: #obj seq) = String.Join(sep, Array.ofSeq str)