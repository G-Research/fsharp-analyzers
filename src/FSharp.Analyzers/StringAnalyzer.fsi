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
              "https://learn.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings")>]
val endsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer("String.StartsWith Analyzer",
              "Verifies the correct usage of System.String.StartsWith",
              "https://learn.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings")>]
val startsWithAnalyzer : ctx : CliContext -> Async<Message list>

[<CliAnalyzer("String.IndexOf Analyzer",
              "Verifies the correct usage of System.String.IndexOf",
              "https://learn.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings")>]
val indexOfAnalyzer : ctx : CliContext -> Async<Message list>
