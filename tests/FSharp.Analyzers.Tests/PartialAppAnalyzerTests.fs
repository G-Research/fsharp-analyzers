namespace GR.FSharp.Analyzers.Tests

module PartialAppAnalyzerTests =

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
            let! options = mkOptionsFromProject "net7.0" []

            projectOptions <- options
        }

    type TestCases() =
        static member DataFolder = Path.Combine (__SOURCE_DIRECTORY__, "..", "data")

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                let testsDirectory = Path.Combine (TestCases.DataFolder, "partialapp")

                Directory.EnumerateFiles (testsDirectory, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<TestCases>)>]
    let PartialAppTests (fileName : string) =
        task {
            let dataFolder = TestCases.DataFolder
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> PartialAppAnalyzer.partialAppCliAnalyzer

            do! assertExpected fileName messages
        }
