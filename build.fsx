#r "nuget: Fun.Build, 0.5.2"

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

tryPrintPipelineCommandHelp ()
