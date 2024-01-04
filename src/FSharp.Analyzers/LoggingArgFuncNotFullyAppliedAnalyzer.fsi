module GR.FSharp.Analyzers.LoggingArgFuncNotFullyAppliedAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code : string = "GRA-LOGARGFUNCFULLAPP-001"

[<Literal>]
val Name : string = "LoggingArgFuncNotFullyAppliedAnalyzer"

[<Literal>]
val ShortDescription : string = "Checks if function arguments to ILogging methods are fully applied"

[<Literal>]
val HelpUri : string = "https://g-research.github.io/fsharp-analyzers/analyzers/LoggingArgFuncNotFullyAppliedAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val loggingArgFuncNotFullyAppliedCliAnalyzer : ctx : CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val loggingArgFuncNotFullyAppliedEditorAnalyzer : ctx : EditorContext -> Async<Message list>
