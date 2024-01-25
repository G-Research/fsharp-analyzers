module GR.FSharp.Analyzers.DisposedBeforeAsyncRunAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-DISPBEFOREASYNC-001"

let getType (checkFileResults : FSharpCheckFileResults) (sourceText : ISourceText) (synPat : SynPat) =
    let lineText = sourceText.GetLineString (synPat.Range.EndLine - 1) // 0-based

    match synPat with
    | SynPat.LongIdent (longDotId = SynLongIdent (id = idents)) -> idents |> List.tryLast
    | SynPat.Named (ident = SynIdent (ident = ident)) -> Some ident
    | _ -> None
    |> Option.map (fun i ->
        checkFileResults.GetSymbolUseAtLocation (synPat.Range.EndLine, synPat.Range.EndColumn, lineText, [ i.idText ])
        |> Option.map (fun symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv when mfv.IsFunction -> mfv.FullTypeSafe
            | _ -> None
        )
    )
    |> Option.flatten
    |> Option.flatten

let asyncOrTask (t : FSharpType) =
    t.GenericArguments
    |> Seq.tryLast
    |> Option.map (fun t ->
        if t.HasTypeDefinition then
            match t.TypeDefinition.TryGetFullName () with
            | Some fullName when fullName.StartsWith ("Microsoft.FSharp.Control.FSharpAsync", StringComparison.Ordinal) ->
                Some "async"
            | Some fullName when fullName.StartsWith ("System.Threading.Tasks.Task", StringComparison.Ordinal) ->
                Some "task"
            | _ -> None
        else
            None
    )
    |> Option.flatten

let pathContainsAsyncOrTaskReturningFunc
    (checkFileResults : FSharpCheckFileResults)
    (sourceText : ISourceText)
    (path : SyntaxVisitorPath)
    =
    path
    |> Seq.tryPick (fun node ->
        match node with
        | SyntaxNode.SynBinding (SynBinding (headPat = headPat)) ->
            getType checkFileResults sourceText headPat |> Option.map asyncOrTask
        | _ -> None
    )
    |> Option.flatten

let pathContainsComputationExpr (path : SyntaxVisitorPath) =
    path
    |> Seq.exists (
        function
        | SyntaxNode.SynExpr (SynExpr.ComputationExpr _) -> true
        | _ -> false
    )

[<Literal>]
let SwitchOffComment = "Note: disposed before returned"

let collectUses (sourceText : ISourceText) (ast : ParsedInput) (checkFileResults : FSharpCheckFileResults) =
    let comments =
        match ast with
        | ParsedInput.ImplFile parsedImplFileInput -> parsedImplFileInput.Trivia.CodeComments
        | _ -> []

    let isSwitchedOffPerCommment (useRange : Range) =
        comments
        |> List.exists (fun c ->
            match c with
            | CommentTrivia.LineComment r ->
                if r.StartLine = useRange.StartLine - 1 then
                    let lineOfComment = sourceText.GetLineString (r.StartLine - 1) // 0-based
                    lineOfComment.Contains (SwitchOffComment, StringComparison.Ordinal)
                else
                    false
            | _ -> false
        )

    let uses = ResizeArray<range * string> ()

    let rec hasAsyncOrTaskInBody (body : SynExpr) =
        match body with
        | SynExpr.App (funcExpr = SynExpr.Ident (ident = ident)) when ident.idText = "async" || ident.idText = "task" ->
            true
        | SynExpr.LetOrUse (body = body) -> hasAsyncOrTaskInBody body
        | SynExpr.Sequential (expr2 = expr2) -> hasAsyncOrTaskInBody expr2
        | _ -> false

    let walker =
        { new SyntaxCollectorBase() with
            override _.WalkExpr (path : SyntaxVisitorPath, synExpr : SynExpr) : unit =

                if not (pathContainsComputationExpr path) then
                    match synExpr with
                    | SynExpr.LetOrUse (isUse = true ; bindings = [ binding ] ; body = body) ->
                        if
                            not (isSwitchedOffPerCommment binding.RangeOfBindingWithoutRhs)
                            && hasAsyncOrTaskInBody body
                        then
                            match pathContainsAsyncOrTaskReturningFunc checkFileResults sourceText path with
                            | Some ce -> uses.Add (binding.RangeOfBindingWithoutRhs, ce)
                            | _ -> ()
                    | _ -> ()
        }

    walkAst walker ast
    uses.ToArray ()

let analyze (sourceText : ISourceText) ast (checkFileResults : FSharpCheckFileResults) =
    let uses = collectUses sourceText ast checkFileResults

    [
        for useRange, ce in uses do
            {
                Type = "DisposedBeforeAsyncRun analyzer"
                Message = $"Object is disposed before returned %s{ce} is run"
                Code = Code
                Severity = Warning
                Range = useRange
                Fixes = []
            }
    ]

[<Literal>]
let Name = "DisposedBeforeAsyncRunAnalyzer"

[<Literal>]
let ShortDescription =
    "Warns about disposed objects before returned asyncs/tasks are run"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/DisposedBeforeAsyncRunAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let disposedBeforeAsyncRunCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async { return analyze ctx.SourceText ctx.ParseFileResults.ParseTree ctx.CheckFileResults }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let disposedBeforeAsyncRunEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) ->
        async {
            return
                ctx.CheckFileResults
                |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }
