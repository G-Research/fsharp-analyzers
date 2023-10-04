namespace GR.FSharp.Analyzers.Tests

module StringAnalyzerTests =

    open System.Collections
    open System.IO
    open FSharp.Compiler.CodeAnalysis
    open NUnit.Framework
    open FSharp.Analyzers.SDK.Testing
    open GR.FSharp.Analyzers
    open GR.FSharp.Analyzers.Tests.Common

    let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

    [<SetUp>]
    let Setup () =
        task {
            let! options = mkOptionsFromProject "net6.0" []
            projectOptions <- options
        }

    type TestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "string" ; "endswith" |]

    [<TestCaseSource(typeof<TestCases>)>]
    let EndsWithTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> StringAnalyzers.endsWithAnalyzer

            do! assertExpected fileName messages
        }

    type NegativeTestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "string" ; "endswith" ; "negative" |]

    [<TestCaseSource(typeof<NegativeTestCases>)>]
    let NegativeEndsWithTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> StringAnalyzers.endsWithAnalyzer

            Assert.IsEmpty messages
        }
