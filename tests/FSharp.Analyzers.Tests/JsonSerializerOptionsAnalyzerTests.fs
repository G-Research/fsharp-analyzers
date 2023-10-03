namespace ``G-Research``.FSharp.Analyzers.Tests

module JsonSerializerOptionsAnalyzerTests =

    open System.Collections
    open System.IO
    open NUnit.Framework
    open FSharp.Compiler.CodeAnalysis
    open FSharp.Analyzers.SDK.Testing
    open ``G-Research``.FSharp.Analyzers
    open Testing

    let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

    [<SetUp>]
    let Setup () =
        task {
            let! options =
                mkOptionsFromProject
                    "net7.0"
                    [
                        {
                            Name = "System.Text.Json"
                            Version = "7.0.3"
                        }
                    ]

            projectOptions <- options
        }

    type TestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                constructTestCaseEnumerator [| "jsonserializeroptions" |]

    [<TestCaseSource(typeof<TestCases>)>]
    let JsonSerializerOptionsTests (fileName : string) =
        task {
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> JsonSerializerOptionsAnalyzer.jsonSerializerOptionsAnalyzer

            do! assertExpected fileName messages
        }
