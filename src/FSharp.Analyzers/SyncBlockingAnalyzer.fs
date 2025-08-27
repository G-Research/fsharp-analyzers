module GR.FSharp.Analyzers.SyncBlockingAnalyzer

open System
open FSharp.Analyzers.Comments
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-SYNCBLOCK-001"

[<Literal>]
let SwitchOffComment = "synchronous blocking call allowed"

let problematicMethods =
    [
        "Microsoft.FSharp.Control.RunSynchronously" // This is how it appears in the TAST
        "Microsoft.FSharp.Control.Async.RunSynchronously"
        "Microsoft.FSharp.Control.FSharpAsync.RunSynchronously"
        "System.Threading.Tasks.Task.Wait"
        "System.Threading.Tasks.Task.WaitAll"
        "System.Threading.Tasks.Task.WaitAny"
        "System.Runtime.CompilerServices.TaskAwaiter.GetResult"
        "System.Runtime.CompilerServices.TaskAwaiter`1.GetResult"
        "System.Runtime.CompilerServices.ValueTaskAwaiter.GetResult"
        "System.Runtime.CompilerServices.ValueTaskAwaiter`1.GetResult"
    ]
    |> Set.ofList

let problematicProperties =
    [
        "System.Threading.Tasks.Task.get_Result" // Note: F# doesn't include `1 in FullName for generic type property getters
        "System.Threading.Tasks.ValueTask.get_Result" // Same here
    ]
    |> Set.ofList

let analyze (sourceText : ISourceText) (ast : ParsedInput) (checkFileResults : FSharpCheckFileResults) =
    let comments =
        match ast with
        | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
        | _ -> []

    let violations = ResizeArray<range * string * string> ()

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ _ (m : range) =

                // Check for regular method calls
                if problematicMethods.Contains mfv.FullName then
                    if not (isSwitchedOffPerComment SwitchOffComment comments sourceText m) then
                        let methodName =
                            if mfv.DisplayName.Contains '.' then
                                mfv.DisplayName
                            else if
                                // Special handling for Async.RunSynchronously
                                mfv.FullName = "Microsoft.FSharp.Control.RunSynchronously"
                            then
                                "Async.RunSynchronously"
                            else
                                // Get more context for better error messages
                                let parts = mfv.FullName.Split '.'

                                if parts.Length >= 2 then
                                    $"{parts.[parts.Length - 2]}.{parts.[parts.Length - 1]}"
                                else
                                    mfv.DisplayName

                        violations.Add (m, "method", methodName)

                // Check for property getters (Result property access)
                elif problematicProperties.Contains mfv.FullName then
                    if not (isSwitchedOffPerComment SwitchOffComment comments sourceText m) then
                        // For property getters, use just the property name
                        violations.Add (m, "property", "Result")
        }

    match checkFileResults.ImplementationFile with
    | Some typedTree -> walkTast walker typedTree
    | None -> ()

    violations
    |> Seq.map (fun (range, _kind, name) ->
        {
            Type = "SyncBlockingAnalyzer"
            Message =
                $"Synchronous blocking call '%s{name}' should be avoided. "
                + "This can cause deadlocks and thread pool starvation. "
                + "Consider using `let!` in a `task` or `async` computation expression. "
                + "Suppress with comment including text 'synchronous blocking call allowed'."
            Code = Code
            Severity = Severity.Warning
            Range = range
            Fixes = []
        }
    )
    |> Seq.toList

[<Literal>]
let Name = "SyncBlockingAnalyzer"

[<Literal>]
let ShortDescription =
    "Bans synchronous blocking operations like Task.Result and Async.RunSynchronously"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/SyncBlockingAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let syncBlockingCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree ctx.CheckFileResults }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let syncBlockingEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) ->
        async {
            return
                ctx.CheckFileResults
                |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }
