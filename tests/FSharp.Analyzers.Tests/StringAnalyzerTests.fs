module ``G-Research``.FSharp.Analyzers.Tests

open System.Collections
open System.IO
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open NUnit.Framework
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing

let writeActual sourceFile messages =
    let actualFile = $"%s{sourceFile}.actual"

    messages
    |> List.map (fun (m : Message) ->
        $"%s{m.Code} | %A{m.Severity} | (%i{m.Range.StartLine},%i{m.Range.StartColumn} - %i{m.Range.EndLine},%i{m.Range.EndColumn}) | %s{m.Message}"
    )
    |> String.concat "\n"
    |> fun contents -> File.WriteAllTextAsync (actualFile, contents)

let shouldUpdateBaseline () =
    System.Environment.GetEnvironmentVariable "TEST_UPDATE_BSL"
    |> Option.ofObj
    |> Option.map (fun v -> v.Trim () = "1")
    |> Option.defaultValue false

let assertExpected sourceFile =
    task {
        let! actual = File.ReadAllTextAsync $"{sourceFile}.actual"
        let expectedFile = $"%s{sourceFile}.expected"

        if shouldUpdateBaseline () then
            do! File.WriteAllTextAsync (expectedFile, actual)

        let! expected =
            if File.Exists expectedFile then
                File.ReadAllTextAsync expectedFile
            else
                Task.FromResult "No baseline was found"

        Assert.AreEqual (expected, actual)
    }

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

        do! writeActual fileName messages
        do! assertExpected fileName
    }
