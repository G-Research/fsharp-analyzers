#r "nuget: Fun.Build, 0.5.2"
#r "nuget: Humanizer.Core"

open System
open System.IO
open System.Xml.Linq
open Fun.Build
open Humanizer

let (</>) a b = Path.Combine (a, b)

let restoreStage =
    stage "restore" {
        run "dotnet tool restore"
        run "dotnet restore --locked-mode"
    }

let buildStage =
    stage "build" { run "dotnet build -c Release --no-restore -maxCpuCount" }

pipeline "Build" {
    restoreStage
    stage "lint" { run "dotnet fantomas . --check" }
    buildStage
    stage "test" { run "dotnet test -c Release --no-build" }

    stage "docs" {
        run "dotnet fsdocs build --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs --eval"
    }

    runIfOnlySpecified false
}

pipeline "EnsureTrailingNewline" {
    stage "TestData" {
        run (fun _ ->
            Directory.EnumerateFiles (
                Path.Combine (__SOURCE_DIRECTORY__, "tests", "FSharp.Analyzers.Tests", "data"),
                "*.*",
                SearchOption.AllDirectories
            )
            |> Seq.iter (fun filePath ->
                let contents = File.ReadAllText filePath
                let contents = contents.Replace ("\r", "")

                let contents =
                    if contents.EndsWith ("\n", StringComparison.Ordinal) then
                        contents
                    else
                        String.Concat (contents, "\n")

                File.WriteAllText (filePath, contents)
            )
        )
    }

    runIfOnlySpecified true
}

pipeline "Docs" {
    stage "Docs" {
        restoreStage
        buildStage

        run
            "dotnet fsdocs watch --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs --eval --port 5000"
    }

    runIfOnlySpecified true
}

let getLastCompileItem (fsproj : string) =
    let xml = File.ReadAllText fsproj
    let doc = XDocument.Parse xml
    Seq.last (doc.Descendants (XName.Get "Compile"))

pipeline "NewAnalyzer" {
    stage "Scaffold" {
        run (fun _ctx ->
            Console.Write ("Enter analyzer name:")
            let analyzerName = Console.ReadLine().Trim ()

            let analyzerName =
                if analyzerName.EndsWith ("Analyzer", StringComparison.Ordinal) then
                    analyzerName
                else
                    $"%s{analyzerName}Analyzer"

            let camelCasedName = analyzerName.Camelize ()

            let analyzerFilePath =
                __SOURCE_DIRECTORY__ </> $"src/FSharp.Analyzers/%s{analyzerName}.fs"

            let analyzerContent =
                $"""module GR.FSharp.Analyzers.%s{analyzerName}

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<CliAnalyzer("%s{analyzerName}",
              "", // TODO: add description.
              "https://g-research.github.io/fsharp-analyzers/analyzers/%s{analyzerName}.html")>]
let %s{camelCasedName} : Analyzer<CliContext> =
    fun (ctx : CliContext) -> async {{ return List.empty<Message> }}
"""

            File.WriteAllText (analyzerFilePath, analyzerContent)
            printfn "Created %s" analyzerFilePath

            let addCompileItem relativeFsProj filenameWithoutExtension =
                let fsproj = __SOURCE_DIRECTORY__ </> relativeFsProj
                let sibling = getLastCompileItem fsproj

                if
                    sibling.Attribute(XName.Get ("Include")).Value
                    <> $"%s{filenameWithoutExtension}.fs"
                then
                    sibling.Parent.Add (XElement.Parse ($"<Compile Include=\"%s{filenameWithoutExtension}.fs\" />"))
                    sibling.Document.Save fsproj

            addCompileItem "src/FSharp.Analyzers/FSharp.Analyzers.fsproj" analyzerName

            let testFolderName = analyzerName.Replace("Analyzer", String.Empty).Camelize ()

            let analyzerTestsFilePath =
                __SOURCE_DIRECTORY__
                </> $"tests/FSharp.Analyzers.Tests/%s{analyzerName}Tests.fs"

            let analyzerTestsContent =
                $"""module GR.FSharp.Analyzers.Tests.%s{analyzerName}Tests

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
    task {{
        let! options = mkOptionsFromProject "net7.0" []
        projectOptions <- options
    }}

type TestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "%s{testFolderName}" |]

[<TestCaseSource(typeof<TestCases>)>]
let %s{analyzerName}Tests (fileName : string) =
    task {{
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> %s{analyzerName}.%s{camelCasedName}

        do! assertExpected fileName messages
    }}

type NegativeTestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "%s{testFolderName}" ; "negative" |]

[<TestCaseSource(typeof<NegativeTestCases>)>]
let NegativeTests (fileName : string) =
    task {{
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> %s{analyzerName}.%s{camelCasedName}

        Assert.That (messages, Is.Empty)
    }}
"""

            File.WriteAllText (analyzerTestsFilePath, analyzerTestsContent)

            addCompileItem "tests/FSharp.Analyzers.Tests/FSharp.Analyzers.Tests.fsproj" $"%s{analyzerName}Tests"
            printfn "Created %s" analyzerTestsFilePath

            let testFolder =
                DirectoryInfo (__SOURCE_DIRECTORY__ </> "tests/data" </> testFolderName)

            testFolder.Create ()
            let sampleFilePath = testFolder.FullName </> "Sample.fs"
            File.WriteAllText (sampleFilePath, "module Sample\n\n")
            printfn "Created %s" sampleFilePath

            let documentationFilePath =
                __SOURCE_DIRECTORY__ </> $"docs/analyzers/%s{analyzerName}.md"

            let title = testFolderName.Pascalize ()

            let documentationContent =
                $"""---
title: %s{title} Analyzer
category: analyzers
categoryindex: 1
index: 
---

# %s{title} Analyzer

## Problem

```fsharp

```

## Fix

```fsharp

```
"""

            File.WriteAllText (documentationFilePath, documentationContent)
        )
    }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
