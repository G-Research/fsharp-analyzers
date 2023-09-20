namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK

module StringAnalyzers =
    [<CliAnalyzer "String.EndsWith Analyzer">]
    val endsWithAnalyzer : ctx : CliContext -> Async<Message list>

    [<CliAnalyzer "String.StartsWith Analyzer">]
    val startsWithAnalyzer : ctx : CliContext -> Async<Message list>

    [<CliAnalyzer "String.IndexOf Analyzer">]
    val indexOfAnalyzer : ctx : CliContext -> Async<Message list>
