namespace GR.FSharp.Analyzers.Tests

module LoggingArgFuncNotFullyAppliedAnalyzerTests =

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
            let! options =
                mkOptionsFromProject
                    framework
                    [
                        {
                            Name = "Microsoft.Extensions.Logging"
                            Version = "8.0.0"
                        }
                        {
                            Name = "Microsoft.Extensions.Logging.Console"
                            Version = "8.0.0"
                        }
                    ]

            projectOptions <- options
        }

    type TestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "loggingargfuncnotfullyapplied" |]

    [<TestCaseSource(typeof<TestCases>)>]
    let LoggingArgFuncNotFullyAppliedTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> LoggingArgFuncNotFullyAppliedAnalyzer.loggingArgFuncNotFullyAppliedCliAnalyzer

            do! assertExpected fileName messages
        }

    type NegativeTestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "loggingargfuncnotfullyapplied" ; "negative" |]

    [<TestCaseSource(typeof<NegativeTestCases>)>]
    let NegativeTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> LoggingArgFuncNotFullyAppliedAnalyzer.loggingArgFuncNotFullyAppliedCliAnalyzer

            Assert.That (messages, Is.Empty)
        }
