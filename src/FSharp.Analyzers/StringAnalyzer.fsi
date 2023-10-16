module GR.FSharp.Analyzers.StringAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val StringEndsWithCode : string = "GRA-STRING-001"

[<Literal>]
val StringStartsWithCode : string = "GRA-STRING-002"

[<Literal>]
val StringIndexOfCode : string = "GRA-STRING-003"

[<CliAnalyzer "String.EndsWith Analyzer">]
val endsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer "String.StartsWith Analyzer">]
val startsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer "String.IndexOf Analyzer">]
val indexOfAnalyzer : ctx : CliContext -> Async<Message list>
