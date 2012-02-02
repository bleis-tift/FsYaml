module Program

type t = { name: string; hr: int; avg: double }

let str = "~"
let result = Yaml.load<string option> str

printfn "%A" result