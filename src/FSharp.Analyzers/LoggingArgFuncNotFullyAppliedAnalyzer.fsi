module GR.FSharp.Analyzers.LoggingArgFuncNotFullyAppliedAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code : string = "GRA-LOGARGFUNCFULLAPP-001"

[<CliAnalyzer("LoggingArgFuncNotFullyAppliedAnalyzer",
              "Checks if function arguments to ILogging methods are fully applied",
              "https://g-research.github.io/fsharp-analyzers/analyzers/LoggingArgFuncNotFullyAppliedAnalyzer.html")>]
val loggingArgFuncNotFullyAppliedAnalyzer : ctx : CliContext -> Async<Message list>
