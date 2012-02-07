module Yaml

open FParsec
open Microsoft.FSharp.Reflection
open System.Text.RegularExpressions

open Patterns
open ReflectionUtils
open YamlDump

type Context = {
  PreIndent: int64
  Indents: int64 list
  PSpaceStack: Parser<unit, Context> list
}
with
  static member make initPSpace = { PreIndent = 0L; Indents = [ 0L ]; PSpaceStack = [ initPSpace ] }

type Parser<'a> = Parser<'a, Context>

let bp x = x

let always x = fun _ -> x

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
let pspaces1 = many1 (choice [ attempt (regex @"([ \t]*\n)*[ \t]+" |>> ignore); pcomment ])
let surround ch = between (pchar ch) (pchar ch)
let pquoted =
  manyChars (noneOf "'" <|> (pstring "''" |>> always '\''))
  |> surround '\''
  |>> Quoted
let pdquoted =
  manyChars (noneOf "\"" <|> (pstring "\\\"" |>> always '"'))
  |> surround '"'
  |>> Str.replaceEscapeSequence
  |>> Quoted
let pstrUntil ends =
  choice [
    pquoted
    pdquoted
    manyChars (noneOf (ends + "#")) |>> Str.trim
  ]
let map f p =
  p >>= fun xs ->
    try
      preturn (f xs)
    with e -> fail e.Message

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

let pyamlmap pelem convF =
  choice [
    attempt (pelem |> pinline '{' '}' |> map convF)
    (pelem |> pblock (preturn ()) |> map convF)
  ]

/// リストをパースするパーサを生成する
let rec plist ty =
  let plistElem ty ends =
    match ty with
    | (ListType _ as ty) -> plist ty |>> unbox
    | MapType (kt, vt) -> pmap kt vt |>> unbox
    | UnionType t -> punion t |>> unbox
    | RecordType ty -> precord ty |>> unbox
    | PrimitiveType | OptionType _ -> pstrUntil ends |>> unbox
  let plist' p =
    choice [
      attempt (p |> pinline '[' ']')
      p |> pblock (pchar '-' >>. pspaces1)
    ]
  let ty = elemType ty
  plist' (plistElem ty) |>> (specialize ty >> unbox)
and pnameval pname valTypeF ends = parse {
  let! name = pname
  let! value =
    match valTypeF name with
    | Some(ListType _ as t) -> plist t |>> box
    | Some(MapType (kt, vt)) -> pmap kt vt |>> box
    | Some(UnionType t) -> punion t |>> box
    | Some(RecordType t) -> precord t |>> box
    | Some(PrimitiveType | OptionType _) -> pstrUntil ends |>> box
    | None -> pzero
  return name, value
 }
/// マップをパースするパーサを生成する
and pmap keyType valType =
  let pname = pstrUntil ":" .>> skipAnyOf ":"
  let pelem = pnameval pname (always (Some valType))
  pyamlmap pelem (toMap keyType valType)
/// 判別共用体をパースするパーサを生成する
and punion ty =
  let getFieldType name =
    let c = FSharpType.GetUnionCases ty |> Array.tryFind (fun c -> c.Name = name)
    c |> Option.map (fun c -> c.GetFields().[0].PropertyType)
  let pname = manyCharsTill anyChar (pchar ':' >>. pspaces1)
  let pfield = pnameval pname getFieldType
  pyamlmap pfield (toUnion ty)
/// レコードをパースするパーサを生成する
and precord ty =
  let getFieldType name =
    let prop = ty |> FSharpType.GetRecordFields |> Array.tryFind (fun p -> p.Name = name)
    prop |> Option.map (fun p -> p.PropertyType)
  let pname = manyCharsTill anyChar (pchar ':' >>. pspaces1)
  let pfield = pnameval pname getFieldType
  pyamlmap pfield (toRecord ty)

let pbody = function
| (ListType _) as t -> plist t
| MapType (kt, vt) -> pmap kt vt
| UnionType t -> punion t
| RecordType t -> precord t
| (PrimitiveType | OptionType _) as t -> pstrUntil "" |>> (convValue t)

let load<'a> yamlStr: 'a =
  let parser = ws >>. pbody typeof<'a> .>> ws .>> followedBy eof
  match yamlStr |> FParsec.CharParsers.runParserOnString parser (Context.make pspace_block) "" with
  | Success(res, _, _) -> unbox res
  | Failure(msg, err, state) -> failwithf "msg: %s\nerr: %A\nstate: %A" msg err state

let dump x = YamlDump.dump 0 x