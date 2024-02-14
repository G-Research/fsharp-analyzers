module GR.FSharp.Analyzers.Tests.TypeAnnotateStringFunctionAnalyzerTests

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
            constructTestCaseEnumerator [| "typeAnnotateStringFunctions" |]

[<TestCaseSource(typeof<TestCases>)>]
let TypeAnnotateStringFunctionsTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> TypeAnnotateStringFunctionAnalyzer.typeAnnotateStringFunctionsCliAnalyzer

        do! assertExpected fileName messages
    }

type NegativeTestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "typeAnnotateStringFunctions" ; "negative" |]

[<TestCaseSource(typeof<NegativeTestCases>)>]
let NegativeTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> TypeAnnotateStringFunctionAnalyzer.typeAnnotateStringFunctionsCliAnalyzer

        Assert.That (messages, Is.Empty)
    }
