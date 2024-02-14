module GR.FSharp.Analyzers.Tests.DisposedBeforeAsyncRunAnalyzerTests

open System.Collections
open System.IO
open NUnit.Framework
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.Testing
open GR.FSharp.Analyzers
open GR.FSharp.Analyzers.Tests.Common

let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! options = mkOptionsFromProject framework []
        projectOptions <- options
    }

type TestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "disposedBeforeAsyncRun" |]

[<TestCaseSource(typeof<TestCases>)>]
let DisposedBeforeAsyncRunAnalyzerTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> DisposedBeforeAsyncRunAnalyzer.disposedBeforeAsyncRunCliAnalyzer

        do! assertExpected fileName messages
    }

type NegativeTestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "disposedBeforeAsyncRun" ; "negative" |]

[<TestCaseSource(typeof<NegativeTestCases>)>]
let NegativeTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> DisposedBeforeAsyncRunAnalyzer.disposedBeforeAsyncRunCliAnalyzer

        Assert.That (messages, Is.Empty)
    }
