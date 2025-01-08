module GR.FSharp.Analyzers.Tests.Common

open System
open System.IO
open System.Collections
open System.Threading.Tasks
open NUnit.Framework
open FSharp.Analyzers.SDK

[<Literal>]
let framework = "net8.0"

let shouldUpdateBaseline () =
    Environment.GetEnvironmentVariable "TEST_UPDATE_BSL"
    |> Option.ofObj
    |> Option.map (fun v -> v.Trim () = "1")
    |> Option.defaultValue false

/// Update by:
/// set TEST_UPDATE_BSL=1
/// $env:TEST_UPDATE_BSL=1
let assertExpected sourceFile messages =
    task {
        let actualContents =
            messages
            |> List.map (fun (m : Message) ->
                let fixes =
                    m.Fixes
                    |> List.map (fun fix ->
                        $"(%i{fix.FromRange.StartLine},%i{fix.FromRange.StartColumn} - %i{fix.FromRange.EndLine},%i{fix.FromRange.EndColumn}) %s{fix.ToText}"
                    )
                    |> String.concat ", "

                $"%s{m.Code} | %A{m.Severity} | (%i{m.Range.StartLine},%i{m.Range.StartColumn} - %i{m.Range.EndLine},%i{m.Range.EndColumn}) | %s{m.Message} | [%s{fixes}]"
            )
            |> String.concat "\n"
            |> fun contents -> String.Concat (contents, "\n")

        let expectedFile = $"%s{sourceFile}.expected"
        let actualFile = $"%s{sourceFile}.actual"

        let! expectedContents =
            if File.Exists expectedFile then
                File.ReadAllTextAsync expectedFile
            else
                Task.FromResult "No baseline was found"

        let areEqual = expectedContents = actualContents

        if shouldUpdateBaseline () then
            do! File.WriteAllTextAsync (expectedFile, actualContents)
        elif not areEqual then
            do! File.WriteAllTextAsync (actualFile, actualContents)
        elif File.Exists actualFile then
            File.Delete actualFile

        Assert.That (actualContents, Is.EqualTo expectedContents)
    }

let dataFolder = Path.Combine (__SOURCE_DIRECTORY__, "data")

let constructTestCaseEnumeratorAux (files : string seq) : IEnumerator =
    files
    |> Seq.map (fun f ->
        let fileName = Path.GetRelativePath (dataFolder, f)
        [| fileName :> obj |]
    )
    |> fun s -> s.GetEnumerator ()

let constructTestCaseEnumerator (subDataPath : string array) =
    let testsDirectory = Path.Combine (dataFolder, Path.Combine subDataPath)

    Directory.EnumerateFiles (testsDirectory, "*.fs")
    |> constructTestCaseEnumeratorAux
