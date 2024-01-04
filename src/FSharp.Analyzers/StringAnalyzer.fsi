module GR.FSharp.Analyzers.StringAnalyzer

open FSharp.Analyzers.SDK

[<Literal>]
val HelpUri : string = "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html"

[<Literal>]
val StringEndsWithCode : string = "GRA-STRING-001"

[<Literal>]
val StringEndsWithName : string = "String.EndsWith Analyzer"

[<Literal>]
val StringEndsWithShortDescription : string = "Verifies the correct usage of System.String.EndsWith"

[<CliAnalyzer(StringEndsWithName, StringEndsWithShortDescription, HelpUri)>]
val endsWithCliAnalyzer : ctx : CliContext -> Async<Message list>

[<EditorAnalyzer(StringEndsWithName, StringEndsWithShortDescription, HelpUri)>]
val endsWithEditorAnalyzer : ctx : EditorContext -> Async<Message list>

[<Literal>]
val StringStartsWithCode : string = "GRA-STRING-002"

[<Literal>]
val StringStartsWithName : string = "String.StartsWith Analyzer"

[<Literal>]
val StringStartsWithShortDescription : string = "Verifies the correct usage of System.String.StartsWith"

[<CliAnalyzer(StringStartsWithName, StringStartsWithShortDescription, HelpUri)>]
val startsWithCliAnalyzer : ctx : CliContext -> Async<Message list>

[<EditorAnalyzer(StringStartsWithName, StringStartsWithShortDescription, HelpUri)>]
val startsWithEditorAnalyzer : ctx : EditorContext -> Async<Message list>

[<Literal>]
val StringIndexOfCode : string = "GRA-STRING-003"

[<Literal>]
val StringIndexOfName : string = "String.IndexOf Analyzer"

[<Literal>]
val StringIndexOfShortDescription : string = "Verifies the correct usage of System.String.IndexOf"

[<CliAnalyzer(StringIndexOfName, StringIndexOfShortDescription, HelpUri)>]
val indexOfCliAnalyzer : ctx : CliContext -> Async<Message list>

[<EditorAnalyzer(StringIndexOfName, StringIndexOfShortDescription, HelpUri)>]
val indexOfEditorAnalyzer : ctx : EditorContext -> Async<Message list>
