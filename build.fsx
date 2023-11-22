#r "nuget: Fun.Build, 0.5.2"

open System.IO
open Fun.Build

let restoreStage =
    stage "restore" {
        run "dotnet tool restore"
        run "dotnet restore --locked-mode"
    }

pipeline "Build" {
    restoreStage
    stage "lint" { run "dotnet fantomas . --check" }
    stage "build" { run "dotnet build -c Release --no-restore -maxCpuCount" }
    stage "test" { run "dotnet test -c Release --no-build" }

    stage "docs" {
        run "dotnet fsdocs build --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs"
    }

    runIfOnlySpecified false
}

pipeline "EnsureTrailingNewline" {
    stage "TestData" {
        run (fun _ ->
            Directory.EnumerateFiles (
                Path.Combine (__SOURCE_DIRECTORY__, "tests", "data"),
                "*.*",
                SearchOption.AllDirectories
            )
            |> Seq.iter (fun filePath ->
                let contents = File.ReadAllText filePath
                let contents = contents.Replace ("\r", "")

                let contents =
                    if contents.EndsWith ("\n") then
                        contents
                    else
                        System.String.Concat (contents, "\n")

                File.WriteAllText (filePath, contents)
            )
        )
    }

    runIfOnlySpecified true
}

pipeline "Docs" {
    stage "Docs" {
        restoreStage

        run
            "dotnet fsdocs watch --parameters fsdocs-collection-name \"G-Research F# Analyzers\" --noapidocs --port 5000"
    }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
