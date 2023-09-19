open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

let checker = FSharpChecker.Create ()

/// Helper to construct an ParsedInput from a code snippet.
let mkTree codeSample =
    let parseFileResults =
        checker.ParseFile (
            "FileName.fs",
            SourceText.ofString codeSample,
            { FSharpParsingOptions.Default with
                SourceFiles = [| "FileName.fs" |]
            }
        )
        |> Async.RunSynchronously

    parseFileResults.ParseTree

[<EntryPoint>]
let main _ =

    mkTree
        """
open System

let f (g:string option) =
    match g with
    | Some w when (w.EndsWith(" ") || w.EndsWith Environment.NewLine) -> 1
    | _ -> 0
"""
    |> ``G-Research``.FSharp.Analyzers.StringAnalyzers.findAllInvocations "EndsWith"
    |> printfn "%A"

    0
