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
                    "net6.0"
                    [
                        {
                            Name = "System.Text.Json"
                            Version = "6.0.8"
                        }
                    ]

            projectOptions <- options
        }

    type TestCases() =
        static member DataFolder = Path.Combine (__SOURCE_DIRECTORY__, "..", "data")

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                let jsonSerializerOptionsTests =
                    Path.Combine (TestCases.DataFolder, "jsonserializeroptions")

                Directory.EnumerateFiles (jsonSerializerOptionsTests, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<TestCases>)>]
    let JsonSerializerOptionsTests (fileName : string) =
        task {
            let dataFolder = TestCases.DataFolder
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> JsonSerializerOptionsAnalyzer.jsonSerializerOptionsAnalyzer

            do! assertExpected fileName messages
        }
