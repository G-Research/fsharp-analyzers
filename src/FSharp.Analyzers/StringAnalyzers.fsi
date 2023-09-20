namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK

module StringAnalyzers =
    [<CliAnalyzer>]
    val endsWithAnalyzer : ctx : CliContext -> Async<Message list>

    [<CliAnalyzer>]
    val startsWithAnalyzer : ctx : CliContext -> Async<Message list>
