#!/usr/bin/env -S dotnet fsi --

#r "nuget: Fun.Build, 1.1.18"
#r "nuget: Humanizer.Core"
#r "nuget: Ionide.KeepAChangelog, 0.2.0"

open System
open System.IO
open System.Xml.Linq
open Fun.Build
open Humanizer
open Ionide.KeepAChangelog
open Ionide.KeepAChangelog.Domain

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

/// Runs the analyzers built by this repository over its own source. Requires buildStage to have run first.
/// The tool exits 0 on warnings, so every GRA- code is escalated to an error to make findings fail the stage.
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

                return!
                    ctx.RunCommand
                        $"dotnet fsharp-analyzers --project \"%s{analyzersProject}\" --analyzers-path \"%s{analyzersPath}\" --treat-as-error \"GRA-*\""
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

let packageId = "G-Research.FSharp.Analyzers"
let packageOutput = __SOURCE_DIRECTORY__ </> "artifacts"

/// The newest entry of CHANGELOG.md: its version, its date and its sections rendered as markdown.
/// The release on GitHub is that entry, so the two cannot come to say different things.
let latestChangelogEntry () : string * DateTime * string =
    let changelog = FileInfo (__SOURCE_DIRECTORY__ </> "CHANGELOG.md")

    let parsed =
        match Parser.parseChangeLog changelog with
        | Error error -> failwithf "Could not parse CHANGELOG.md: %A" error
        | Ok result -> result

    let version, date, data =
        match parsed.Releases with
        | [] -> failwith "CHANGELOG.md has no release entry."
        | releases -> releases |> List.maxBy (fun (_, date, _) -> date)

    let body =
        match data with
        | None -> failwith "The newest CHANGELOG.md entry has no sections."
        | Some data ->

        [
            "Added", data.Added
            "Changed", data.Changed
            "Fixed", data.Fixed
            "Deprecated", data.Deprecated
            "Removed", data.Removed
            "Security", data.Security
            yield! Map.toList data.Custom
        ]
        |> List.choose (fun (header, lines) ->
            if String.IsNullOrWhiteSpace lines then
                None
            else
                Some $"### %s{header}\n%s{lines.Trim ()}"
        )
        |> String.concat "\n\n"

    string version, date, body

/// "September 10th Release", the title fantomas and telplin give their releases as well.
let releaseTitle (date : DateTime) : string =
    $"""%s{date.ToString "MMMM"} %s{date.Day.Ordinalize ()} Release"""

/// Create the GitHub release for the newest changelog entry, unless it exists already. A rerun of
/// the workflow, or a push that touches nothing in the changelog, then changes nothing.
let createGithubRelease (ctx : Internal.StageContext) : Async<int> =
    async {
        let version, date, body = latestChangelogEntry ()
        let tag = $"v%s{version}"

        let! existing = ctx.RunCommandCaptureOutput $"gh release view %s{tag} --json tagName"

        match existing with
        | Ok _ ->
            printfn $"Release %s{tag} already exists on GitHub, nothing to do."
            return 0
        | Error _ ->

        let notes =
            $"""# %s{version}

%s{body}

[https://www.nuget.org/packages/%s{packageId}/%s{version}](https://www.nuget.org/packages/%s{packageId}/%s{version})
"""

        let notesFile = Path.GetTempFileName ()
        File.WriteAllText (notesFile, notes)
        let package = packageOutput </> $"%s{packageId}.%s{version}.nupkg"
        let prerelease = if version.Contains '-' then " --prerelease" else ""

        let! result =
            ctx.RunCommand
                $"gh release create %s{tag} \"%s{package}\"%s{prerelease} --title \"%s{releaseTitle date}\" --notes-file \"%s{notesFile}\""

        File.Delete notesFile

        match result with
        | Ok () ->
            printfn $"Created GitHub release %s{tag}."
            return 0
        | Error error ->
            eprintfn $"Could not create GitHub release %s{tag}: %s{error}"
            return 1
    }

// Push the packed nupkg in `artifacts/` to NuGet, then create the matching GitHub release.
// Stages run in order and the pipeline stops at the first failure, so no release is created
// when the push fails. Needs NUGET_KEY and GH_TOKEN in the environment.
pipeline "Release" {
    stage "push" {
        workingDir packageOutput

        run (fun ctx ->
            let apiKey = Environment.GetEnvironmentVariable "NUGET_KEY"

            ctx.RunSensitiveCommand
                $"dotnet nuget push {packageId}.*.nupkg --source https://api.nuget.org/v3/index.json --api-key {apiKey} --skip-duplicate"
        )
    }

    stage "release" { run createGithubRelease }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
