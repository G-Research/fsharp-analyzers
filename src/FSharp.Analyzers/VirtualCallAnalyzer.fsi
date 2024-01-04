module GR.FSharp.Analyzers.VirtualCallAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val Code : string = "GRA-VIRTUALCALL-001"

[<Literal>]
val Name : string = "VirtualCall Analyzer"

[<Literal>]
val ShortDescription : string = "Checks if calls of Seq functions can be replaced with functions from the collection modules"

[<Literal>]
val HelpUri : string = "https://g-research.github.io/fsharp-analyzers/analyzers/VirtualCallAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
val virtualCallCliAnalyzer : ctx : CliContext -> Async<Message list>

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
val virtualCallEditorAnalyzer : ctx : EditorContext -> Async<Message list>
