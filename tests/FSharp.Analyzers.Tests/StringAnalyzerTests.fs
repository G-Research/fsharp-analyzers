namespace ``G-Research``.FSharp.Analyzers.Tests

module StringAnalyzerTests =

    open System.Collections
    open System.IO
    open FSharp.Compiler.CodeAnalysis
    open NUnit.Framework
    open FSharp.Analyzers.SDK
    open FSharp.Analyzers.SDK.Testing
    open ``G-Research``.FSharp.Analyzers
    open Testing

    let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

    [<SetUp>]
    let Setup () =
        task {
            let! options = mkOptionsFromProject "net6.0" []
            projectOptions <- options
        }

    type TestCases() =
        static member DataFolder = Path.Combine (__SOURCE_DIRECTORY__, "..", "data")

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                let endsWithTests = Path.Combine (TestCases.DataFolder, "string", "endswith")

                Directory.EnumerateFiles (endsWithTests, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<TestCases>)>]
    let EndsWithTests (fileName : string) =
        task {
            let dataFolder = TestCases.DataFolder
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
                let endsWithTests =
                    Path.Combine (TestCases.DataFolder, "string", "endswith", "negative")

                Directory.EnumerateFiles (endsWithTests, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<NegativeTestCases>)>]
    let NegativeEndsWithTests (fileName : string) =
        task {
            let fileName = Path.Combine (TestCases.DataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> StringAnalyzers.endsWithAnalyzer

            Assert.IsEmpty messages
        }
