module YamlDump

open System.Text.RegularExpressions
open System.Text

let dumpString str =
  let pat = @"[\b\r\n\t""]"
  if(Regex.IsMatch(str, pat)) then
    let ret = new StringBuilder()
    ret.Append(@"""").Append(Regex.Replace(str, @"""", @"\""")).Append(@"""") |> ignore
    ret.ToString()
  else
    str

let dump obj = dumpString obj
  