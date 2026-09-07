#!/usr/bin/env -S dotnet fsi --

#r "nuget: Fun.Build, 1.1.18"
#r "nuget: Humanizer.Core"

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
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

let analyzersProject =
    __SOURCE_DIRECTORY__ </> "src/FSharp.Analyzers/FSharp.Analyzers.fsproj"

/// Every analyzer code (GRA-XXX-000) declared in the analyzer sources.
let analyzerCodes () =
    let codes = HashSet<string> ()

    for file in Directory.EnumerateFiles (Path.GetDirectoryName analyzersProject, "*.fs") do
        for m in Regex.Matches (File.ReadAllText file, "\"(GRA-[A-Z0-9-]+)\"") do
            codes.Add m.Groups.[1].Value |> ignore

    codes

/// Runs the analyzers built by this repository over its own source. Requires buildStage to have run first.
/// The tool exits 0 on warnings, so every known code is escalated to an error to make findings fail the stage.
let analyzeStage =
    stage "analyze" {
        run (fun ctx ->
            async {
                // Ask MSBuild for the built assembly rather than searching bin/Release recursively:
                // a stale output folder for an older target framework would make the tool fail to load.
                let! targetPath =
                    ctx.RunCommandCaptureOutput
                        $"dotnet msbuild \"%s{analyzersProject}\" -getProperty:TargetPath -p:Configuration=Release"

                match targetPath with
                | Error error -> return Error error
                | Ok targetPath ->

                let analyzersPath = Path.GetDirectoryName (targetPath.Trim ())
                let treatAsError = analyzerCodes () |> String.concat " "

                return!
                    ctx.RunCommand
                        $"dotnet fsharp-analyzers --project \"%s{analyzersProject}\" --analyzers-path \"%s{analyzersPath}\" --treat-as-error %s{treatAsError}"
            }
        )
    }

pipeline "Build" {
    restoreStage
    stage "lint" { run "dotnet fantomas . --check" }
    buildStage
    stage "test" { run "dotnet test -c Release --no-build" }
    analyzeStage

    stage "docs" {
        run "dotnet fsdocs build --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs --eval"
    }

    runIfOnlySpecified false
}

pipeline "Analyze" {
    restoreStage
    buildStage
    analyzeStage

    runIfOnlySpecified true
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
            "dotnet fsdocs watch --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs --eval --port 5007"
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
            Console.Write "Enter analyzer name:"
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
                DirectoryInfo (__SOURCE_DIRECTORY__ </> "tests/FSharp.Analyzers.Tests/data" </> testFolderName)

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
