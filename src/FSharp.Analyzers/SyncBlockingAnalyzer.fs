module GR.FSharp.Analyzers.SyncBlockingAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<CliAnalyzer("SyncBlockingAnalyzer",
              "", // TODO: add description.
              "https://g-research.github.io/fsharp-analyzers/analyzers/SyncBlockingAnalyzer.html")>]
let syncBlockingAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) -> async { return List.empty<Message> }
