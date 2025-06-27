module GR.FSharp.Analyzers.Tests.StringAnalyzerTests

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
        let! options = mkOptionsFromProject framework []
        projectOptions <- options
    }

type TestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            [| "endswith" ; "indexof" ; "startswith" ; "lastindexof" |]
            |> Seq.collect (fun subFolder ->
                let folder = Path.Combine (dataFolder, "string", subFolder)
                Directory.EnumerateFiles (folder, "*.fs")
            )
            |> constructTestCaseEnumeratorAux

let findStringAnalyzerFor (fileName : string) =
    fileName.Split Path.DirectorySeparatorChar
    |> Array.skip 1
    |> Array.head
    |> function
        | "endswith" -> StringAnalyzer.endsWithCliAnalyzer
        | "startswith" -> StringAnalyzer.startsWithCliAnalyzer
        | "indexof" -> StringAnalyzer.indexOfCliAnalyzer
        | "lastindexof" -> StringAnalyzer.lastIndexOfCliAnalyzer
        | unknown -> failwithf $"Unknown subfolder \"%s{unknown}\", please configure analyzer"

[<TestCaseSource(typeof<TestCases>)>]
let StringTests (fileName : string) =
    task {
        let fullPath = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fullPath
            |> getContext projectOptions
            |> findStringAnalyzerFor fileName

        do! assertExpected fullPath messages
    }

type NegativeTestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            [| "endswith" ; "indexof" ; "startswith" ; "lastindexof" |]
            |> Seq.collect (fun subFolder ->
                let folder = Path.Combine (dataFolder, "string", subFolder, "negative")
                Directory.EnumerateFiles (folder, "*.fs")
            )
            |> constructTestCaseEnumeratorAux

[<TestCaseSource(typeof<NegativeTestCases>)>]
let NegativeStringTests (fileName : string) =
    task {
        let fullPath = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fullPath
            |> getContext projectOptions
            |> findStringAnalyzerFor fileName

        Assert.That (messages, Is.Empty)
    }
