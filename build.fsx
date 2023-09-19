#r "nuget: Fun.Build, 0.5.2"

open System.IO
open Fun.Build

pipeline "Build" {
    stage "restore" {
        run "dotnet tool restore"
        run "dotnet restore --locked-mode"
    }

    stage "lint" { run "dotnet fantomas . --check" }
    stage "build" { run "dotnet build -c Release --no-restore -maxCpuCount" }
    stage "test" { run "dotnet test -c Release --no-build" }
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

tryPrintPipelineCommandHelp ()
