namespace GR.FSharp.Analyzers.Tests

module VirtualCallAnalyzerTests =

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
                constructTestCaseEnumerator [| "virtualcall" |]

    [<TestCaseSource(typeof<TestCases>)>]
    let VirtualCallTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> VirtualCallAnalyzer.virtualCallCliAnalyzer

            do! assertExpected fileName messages
        }

    type NegativeTestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "virtualcall" ; "negative" |]

    [<TestCaseSource(typeof<NegativeTestCases>)>]
    let NegativeTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> VirtualCallAnalyzer.virtualCallCliAnalyzer

            Assert.That (messages, Is.Empty)
        }
