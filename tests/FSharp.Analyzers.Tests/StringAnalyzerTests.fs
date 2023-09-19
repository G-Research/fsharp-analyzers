module ``G-Research``.FSharp.Analyzers.Tests

open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing

let assertSingleMessage (severity : Severity) (code : string) (messages : Async<Message list>) =
    async {
        let! messages = messages

        match messages with
        | [ message ] ->
            Assert.AreEqual (severity, message.Severity)
            Assert.AreEqual (code, message.Code)
        | messages -> Assert.Fail $"Expected a single message, got %A{messages}"
    }

let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! options = mkOptionsFromProject "net6.0" []
        projectOptions <- options
    }

[<Test>]
let ``EndsWith call on constant string with one string argument`` () =
    "
\"foo\".EndsWith(\"p\")
"
    |> getContext projectOptions
    |> StringAnalyzers.endsWithAnalyzer
    |> assertSingleMessage Severity.Warning StringAnalyzers.EndsWithCode
