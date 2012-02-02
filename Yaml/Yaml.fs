module Yaml

open FParsec
open Microsoft.FSharp.Reflection
open System.Text.RegularExpressions

open Patterns
open ReflectionUtils

type Context = {
  PreIndent: int64
  Indents: int64 list
  PSpaceStack: Parser<unit, Context> list
}
with
  static member make initPSpace = { PreIndent = 0L; Indents = [ 0L ]; PSpaceStack = [ initPSpace ] }

type Parser<'a> = Parser<'a, Context>

let bp x = x

// 基本的なパーサ
let pcomment = regex @"#[^\n]*" .>> (followedBy (newline |>> ignore <|> eof)) |>> ignore <?> "comment"
let pspace_inline = choice [ attempt (anyOf " \t" |>> ignore); pcomment ] <?> "space or tab or comment"
let pspace_block = choice [ attempt (anyOf " \t\n" |>> ignore); pcomment ] <?> "space or tab or newline or comment"
let ws = parse {
  let! state = getUserState
  let pspace = state.PSpaceStack.Head
  do! many pspace |>> ignore
  return ()
}
let pchar_ws ch = pchar ch .>> ws
let pint_ws = pint32 .>> many (anyOf [' '; '\t'])
let pdouble_ws =
  choice [
    attempt (pchar '0' |>> float)
    attempt (pstring ".inf" |>> fun _ -> infinity)
    attempt (pstring "-.inf" |>> fun _ -> -(infinity))
    attempt (pstring ".nan" |>> fun _ -> nan)
    regex @"-?(0|[1-9][0-9]*)(\.[0-9]*)?([eE][-+]?[0-9]+)?" |>> (fun x -> printfn "!!!%A" x; x) |>> float
  ] .>> many (anyOf [' '; '\t'])
let pspaces1 = many1 (choice [ attempt (regex @"([ \t]*\n)*[ \t]+" |>> ignore); pcomment ])
let surround ch = between (pchar ch) (pchar ch)
let pquoted =
  manyChars (noneOf "'" <|> (pstring "''" |>> fun _ -> '\''))
  |> surround '\''
let pdquoted =
  manyChars (noneOf "\"" <|> (pstring "\\\"" |>> fun _ -> '"'))
  |> surround '"'
  |>> fun str ->
    Regex.Replace(str, @"(?:\\b|\\n|\\r|\\t|\\x([0-9a-fA-F]{2})|\\u([0-9a-fA-F]{4})|\\u([0-9a-fA-F]{8})|\\\\)", fun (m: Match) ->
      match m.Value, m.Length with
      | @"\b", _ -> "\b"
      | @"\n", _ -> "\n"
      | @"\r", _ -> "\r"
      | @"\t", _ -> "\t"
      | @"\\", _ -> "\\"
      | v, _ when v.StartsWith(@"\x") ->
        let num = System.Convert.ToInt32((m.Groups.Item 1).Value, 16)
        System.Char.ConvertFromUtf32(num)
      | _, n ->
        let idx = if n = @"\u0000".Length then 2 else 3
        let num = System.Convert.ToInt32((m.Groups.Item idx).Value, 16)
        System.Char.ConvertFromUtf32(num)
    )
let pstr ends =
  choice [
    pquoted
    pdquoted
    manyChars (noneOf (ends + "#")) |>> fun s -> s.Trim()
  ]

let pushPSpace p c = { c with PSpaceStack = p::c.PSpaceStack }
let popPSpace c = { c with PSpaceStack = c.PSpaceStack.Tail }

/// startChから始まり、endChで終わる、カンマで区切られたpの連続
let pinline startCh endCh p = parse {
  let sep = ','
  let ends = string sep + string endCh
  let! state = getUserState
  do! updateUserState (pushPSpace pspace_inline)
  let! result =
    sepBy (p ends) (pchar_ws sep)
    |> between (pchar_ws startCh) (pchar_ws endCh)
  do! updateUserState popPSpace
  return result
}
/// pprefixで始まる、行ごとのpの連続
let pblock pprefix p =
  let tryParse p =
    choice [
      attempt p |>> Some
      preturn None
    ]
  let rec pblock' p res = parse {
    let! e = tryParse p
    match e with
    | None -> return res
    | Some e ->
      do! ws
      let! r = pblock' p (e::res)
      return r
  }
  let indentIndent indent c = { c with PreIndent = indent; Indents = c.PreIndent::c.Indents }
  let deindentIndent c = { c with PreIndent = c.Indents.Head; Indents = c.Indents.Tail }
  let psuffix level = parse {
    let! state = getUserState
    do! fun stream ->
      if state.Indents.Head <= level then Reply(()) else Reply(ReplyStatus.Error, messageError "")
    do! updateUserState deindentIndent
    return ()
  }
  let pindentContinue pre crnt = fun stream ->
    if pre <= crnt then Reply(()) else Reply(ReplyStatus.Error, messageError "")
  let pprefix' = parse {
    let crnt = ref 0L
    let! { PreIndent = preIndent; Indents = indents } =
      getUserState .>> fun stream -> crnt := stream.Column - 1L; Reply(())
    let indent = match indents with indent::_ -> indent | [] -> failwith "oops!"
    do! pindentContinue preIndent !crnt
    do! pprefix |>> ignore
    return ()
  }
  parse {
    let! { PreIndent = preIndent; Indents = indents } as state = getUserState
    do! updateUserState (pushPSpace pspace_block)
    let indent = match indents with indent::_ -> indent | [] -> failwith "oops!"
    let newIndent = ref indent
    do! (fun stream ->
      newIndent := stream.Column - 1L
      Reply(())
    )
    do! updateUserState (indentIndent !newIndent)
    let! result = pblock' (pprefix' >>. (p "\n")) [] |>> List.rev
    do! psuffix !newIndent
    do! updateUserState popPSpace
    return result
  }

/// ty型のリストをパースするパーサを生成する
let rec plist ty =
  let plistElem ty ends =
    match ty with
    | IntType -> pint_ws |>> unbox
    | DoubleType -> pdouble_ws |>> unbox
    | StrType -> pstr ends |>> unbox
    | OtherType (ListType ty) -> plist ty |>> unbox
    | OtherType (RecordType ty) -> precord ty |>> unbox
    | OtherType (PrimitiveType _) -> failwithf "%s is not supported type." ty.Name
  let plist' p =
    choice [
      attempt (p |> pinline '[' ']')
      p |> pblock (pchar '-' >>. pspaces1)
    ]
  plist' (plistElem ty) |>> (specialize ty >> unbox)
/// レコードをパースするパーサを生成する
and precord ty =
  let precord' p =
    let msg = ref ""
    parse {
      let! xs = p
      try
        return xs |> toRecord ty
      with e -> msg := e.Message
    } <?> !msg
  
  let getFieldType name =
    let prop = ty |> FSharpType.GetRecordFields |> Array.tryFind (fun p -> p.Name = name)
    prop |> Option.map (fun p -> p.PropertyType)
  let pfield ends = parse {
    let! name = manyCharsTill anyChar (pchar ':' >>. pspaces1)
    let! value =
      match getFieldType name with
      | Some(ListType t) -> plist t |>> box
      | Some(RecordType t) -> precord t |>> box
      | Some(PrimitiveType Int) -> pint_ws |>> box
      | Some(PrimitiveType Double) -> pdouble_ws |>> box
      | Some(PrimitiveType Str) -> pstr ends |>> box
      | None -> pzero
    return name, value
  }
  choice [
    attempt (pfield |> pinline '{' '}' |> precord')
    (pfield |> pblock (preturn ()) |> precord')
  ]

let pbody = function
| ListType t -> plist t
| RecordType t -> precord t
| PrimitiveType Int -> pint_ws |>> unbox
| PrimitiveType Double -> pdouble_ws |>> unbox
| PrimitiveType Str -> manyChars anyChar |>> (fun s -> unbox (s.Trim()))

let load<'a> yamlStr: 'a =
  let parser = ws >>. pbody typeof<'a> .>> ws .>> followedBy eof
  match yamlStr |> FParsec.CharParsers.runParserOnString parser (Context.make pspace_block) "" with
  | Success(res, _, _) -> unbox res
  | Failure(msg, err, state) -> failwithf "msg: %s\nerr: %A\nstate: %A" msg err state