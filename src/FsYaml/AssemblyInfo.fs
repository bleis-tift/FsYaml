namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FsYaml")>]
[<assembly: AssemblyProductAttribute("FsYaml")>]
[<assembly: AssemblyDescriptionAttribute("Type safe Yaml library for F#.")>]
[<assembly: AssemblyVersionAttribute("2.0.1")>]
[<assembly: AssemblyFileVersionAttribute("2.0.1")>]
[<assembly: InternalsVisibleToAttribute("FsYaml.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.1"
