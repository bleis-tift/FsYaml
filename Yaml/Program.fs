module Program

type t = { name: string; hr: int; avg: double }

let str =
        "-\n" +
        "  name: Mark McGwire\n" +
        "  hr:   65\n" +
        "  avg:  .inf\n" +
        "-\n" +
        "  name: Sammy Sosa\n" +
        "  hr:   63\n" +
        "  avg:  0.288"
let result = Yaml.load<t list> str

printfn "%A" result