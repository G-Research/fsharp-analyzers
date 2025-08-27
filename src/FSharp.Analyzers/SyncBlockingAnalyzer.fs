module GR.FSharp.Analyzers.SyncBlockingAnalyzer

open System
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
        "System.Threading.Tasks.Task`1.Result"
        "System.Threading.Tasks.ValueTask`1.Result"
    ]
    |> Set.ofList

let isSwitchedOffPerComment (sourceText: ISourceText) (comments: CommentTrivia list) (range: Range) =
    comments
    |> List.exists (fun c ->
        match c with
        | CommentTrivia.LineComment r ->
            // Check same line or line above
            if r.StartLine = range.StartLine || r.StartLine = range.StartLine - 1 then
                let lineOfComment = sourceText.GetLineString(r.StartLine - 1) // 0-based
                lineOfComment.Contains(SwitchOffComment, StringComparison.OrdinalIgnoreCase)
            else
                false
        | CommentTrivia.BlockComment r ->
            // Check if block comment is on same line or line above
            if r.StartLine = range.StartLine || r.EndLine = range.StartLine - 1 then
                let startLine = sourceText.GetLineString(r.StartLine - 1)
                startLine.Contains(SwitchOffComment, StringComparison.OrdinalIgnoreCase)
            else
                false
    )

let analyze (sourceText: ISourceText) (ast: ParsedInput) (checkFileResults: FSharpCheckFileResults) =
    let comments =
        match ast with
        | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
        | _ -> []

    let violations = ResizeArray<range * string * string>()

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (mfv: FSharpMemberOrFunctionOrValue) _ _ _ (m: range) =
                if problematicMethods.Contains mfv.FullName then
                    if not (isSwitchedOffPerComment sourceText comments m) then
                        let methodName =
                            if mfv.DisplayName.Contains(".") then
                                mfv.DisplayName
                            else
                                // Get more context for better error messages
                                let parts = mfv.FullName.Split('.')
                                if parts.Length >= 2 then
                                    $"{parts.[parts.Length - 2]}.{parts.[parts.Length - 1]}"
                                else
                                    mfv.DisplayName
                        violations.Add(m, "method", methodName)

            override _.WalkFSharpFieldGet _ _ (field: FSharpField) =
                // Handle Result property access
                let fullName =
                    match field.DeclaringEntity with
                    | Some entity -> $"{entity.FullName}.{field.Name}"
                    | None -> field.Name

                if problematicProperties.Contains fullName then
                    let range = field.DeclarationLocation
                    if not (isSwitchedOffPerComment sourceText comments range) then
                        violations.Add(range, "property", field.Name)
        }

    match checkFileResults.ImplementationFile with
    | Some typedTree -> walkTast walker typedTree
    | None -> ()

    violations
    |> Seq.map (fun (range, _kind, name) ->
        {
            Type = "SyncBlockingAnalyzer"
            Message =
                $"Synchronous blocking call '%s{name}' should be avoided. " +
                "This can cause deadlocks and thread pool starvation. " +
                "Consider using `let!` in a `task` or `async` computation expression. " +
                "Suppress with comment including text 'synchronous blocking call allowed'."
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
let syncBlockingCliAnalyzer: Analyzer<CliContext> =
    fun (ctx: CliContext) ->
        async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree ctx.CheckFileResults }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let syncBlockingEditorAnalyzer: Analyzer<EditorContext> =
    fun (ctx: EditorContext) ->
        async {
            return
                ctx.CheckFileResults
                |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }