module GR.FSharp.Analyzers.VirtualCallAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code : string = "GRA-VIRTUALCALL-001"

[<CliAnalyzer("VirtualCall Analyzer",
              "Checks if calls of Seq functions can be replaced with functions from the collection modules",
              "https://g-research.github.io/fsharp-analyzers/analyzers/VirtualCallAnalyzer.html")>]
val virtualCallAnalyzer : ctx : CliContext -> Async<Message list>
