namespace ``G-Research``.FSharp.Analyzers.Tests

module UnionCaseAnalyzerTests =

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
            let! options = mkOptionsFromProject "net7.0" []

            projectOptions <- options
        }

    type TestCases() =
        static member DataFolder = Path.Combine (__SOURCE_DIRECTORY__, "..", "data")

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                let testsDirectory = Path.Combine (TestCases.DataFolder, "unioncase")

                Directory.EnumerateFiles (testsDirectory, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<TestCases>)>]
    let UnionCaseTests (fileName : string) =
        task {
            let dataFolder = TestCases.DataFolder
            let fileName = Path.Combine (dataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> UnionCaseAnalyzer.unionCaseAnalyzer

            do! assertExpected fileName messages
        }

    let constructTestCaseEnumerator directoryPath =
        Directory.EnumerateFiles (directoryPath, "*.fs")
        |> Seq.map (fun f ->
            let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
            [| fileName :> obj |]
        )
        |> fun s -> s.GetEnumerator ()

    type NegativeTestCases() =

        interface IEnumerable with
            member _.GetEnumerator () : IEnumerator =
                let testsDirectory = Path.Combine (TestCases.DataFolder, "unioncase", "negative")

                Directory.EnumerateFiles (testsDirectory, "*.fs")
                |> Seq.map (fun f ->
                    let fileName = Path.GetRelativePath (TestCases.DataFolder, f)
                    [| fileName :> obj |]
                )
                |> fun s -> s.GetEnumerator ()

    [<TestCaseSource(typeof<NegativeTestCases>)>]
    let NegativeTests (fileName : string) =
        task {
            let fileName = Path.Combine (TestCases.DataFolder, fileName)

            let! messages =
                File.ReadAllText fileName
                |> getContext projectOptions
                |> UnionCaseAnalyzer.unionCaseAnalyzer

            Assert.IsEmpty messages
        }
