namespace ``G-Research``.FSharp.Analyzers.Tests

module Testing =

    open System.IO
    open System.Threading.Tasks
    open NUnit.Framework
    open FSharp.Analyzers.SDK
    open FSharp.Analyzers.SDK.Testing

    let shouldUpdateBaseline () =
        System.Environment.GetEnvironmentVariable "TEST_UPDATE_BSL"
        |> Option.ofObj
        |> Option.map (fun v -> v.Trim () = "1")
        |> Option.defaultValue false

    let assertExpected sourceFile messages =
        task {
            let actualContents =
                messages
                |> List.map (fun (m : Message) ->
                    $"%s{m.Code} | %A{m.Severity} | (%i{m.Range.StartLine},%i{m.Range.StartColumn} - %i{m.Range.EndLine},%i{m.Range.EndColumn}) | %s{m.Message}"
                )
                |> String.concat "\n"
                |> fun contents -> System.String.Concat (contents, "\n")

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

            Assert.AreEqual (expectedContents, actualContents)
        }
