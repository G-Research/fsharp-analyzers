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

    let uses = ResizeArray<range> () // range of use expressions in async/task returning functions outside of CE
    let asyncOrTaskReturningFunctions = ResizeArray<range * string> () // range and CE name of async/task returning functions

    let walker =
        { new SyntaxCollectorBase() with
            override _.WalkExpr (path : SyntaxVisitorPath, synExpr : SynExpr) : unit =
                if not (pathContainsComputationExpr path) then
                    match synExpr with
                    | SynExpr.LetOrUse (isUse = true ; bindings = [ binding ]) ->
                        if not (isSwitchedOffPerCommment binding.RangeOfBindingWithoutRhs) then
                            uses.Add binding.RangeOfBindingWithoutRhs
                    | _ -> ()

            override _.WalkBinding (_path : SyntaxVisitorPath, binding : SynBinding) : unit =
                let (SynBinding (headPat = headPat)) = binding

                match getType checkFileResults sourceText headPat |> Option.map asyncOrTask with
                | Some asyOrTsk ->
                    match asyOrTsk with
                    | Some ce -> asyncOrTaskReturningFunctions.Add (binding.RangeOfBindingWithRhs, ce)
                    | None -> ()
                | None -> ()
        }

    walkAst walker ast
    uses.ToArray (), asyncOrTaskReturningFunctions.ToArray ()

let analyze (sourceText : ISourceText) ast (checkFileResults : FSharpCheckFileResults) =
    let uses, asyncOrTaskReturningFunctions =
        collectUses sourceText ast checkFileResults

    seq {
        for funcRange, ce in asyncOrTaskReturningFunctions do
            let useRanges = uses |> Array.filter (Range.rangeContainsRange funcRange)

            for useRange in useRanges do
                let msg =
                    {
                        Type = "DisposedBeforeAsyncRun analyzer"
                        Message = $"Object is disposed before returned %s{ce} is run"
                        Code = Code
                        Severity = Warning
                        Range = useRange
                        Fixes = []
                    }

                yield msg
    }
    |> Seq.toList
    |> List.distinct // for cases like UseBeforeAsyncInNestedFunc.fs

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
