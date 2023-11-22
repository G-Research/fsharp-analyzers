module GR.FSharp.Analyzers.StringAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val StringEndsWithCode : string = "GRA-STRING-001"

[<Literal>]
val StringStartsWithCode : string = "GRA-STRING-002"

[<Literal>]
val StringIndexOfCode : string = "GRA-STRING-003"

[<CliAnalyzer("String.EndsWith Analyzer",
              "Verifies the correct usage of System.String.EndsWith",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
val endsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer("String.StartsWith Analyzer",
              "Verifies the correct usage of System.String.StartsWith",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
val startsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer("String.IndexOf Analyzer",
              "Verifies the correct usage of System.String.IndexOf",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
val indexOfAnalyzer : ctx : CliContext -> Async<Message list>
