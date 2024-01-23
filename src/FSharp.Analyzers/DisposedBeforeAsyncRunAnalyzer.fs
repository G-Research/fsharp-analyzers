module GR.FSharp.Analyzers.DisposedBeforeAsyncRunAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns

[<Literal>]
let Code = "GRA-DISPBEFOREASYNC-001"

let collectUses (ast : ParsedInput) (sourceText : ISourceText) =
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
                    lineOfComment.Contains ("Note: disposed before returned", StringComparison.Ordinal)
                else
                    false
            | _ -> false
        )

    let state = ResizeArray<range> ()

    let walker =
        { new SyntaxCollectorBase() with
            override _.WalkExpr (_path : SyntaxVisitorPath, synExpr : SynExpr) : unit =
                match synExpr with
                | SynExpr.LetOrUse (isUse = true ; bindings = [ binding ]) ->
                    if not (isSwitchedOffPerCommment binding.RangeOfBindingWithoutRhs) then
                        state.Add (binding.RangeOfBindingWithoutRhs)
                | _ -> ()

                ()
        }

    walkAst walker ast
    state.ToArray ()

let getUseRanges (uses : Range array) (expr : FSharpExpr) =
    let ranges = ResizeArray<Range> ()

    let rec traverse (expr : FSharpExpr) =
        match expr with
        | Let ((bindingVar, _bindingExpr, _debugPointAtBinding), bodyExpr) ->
            let rangesToAdd =
                uses
                |> Array.filter (fun r -> Range.rangeContainsRange r bindingVar.DeclarationLocation)

            ranges.AddRange rangesToAdd
            traverse bodyExpr
        | TryFinally (fsharpExpr, _FSharpExpr, _debugPointAtTry, _debugPointAtFinally) -> traverse fsharpExpr
        | _ -> ()

    traverse expr
    ranges.ToArray ()

let analyzeAsyncOrTaskReturningDecls (uses : Range array) (decl : FSharpImplementationFileDeclaration) =
    let state = ResizeArray<string * range> ()

    let rec traverseDecl (decl : FSharpImplementationFileDeclaration) =
        match decl with
        | FSharpImplementationFileDeclaration.Entity (declarations = declarations) ->
            declarations |> List.iter (traverseDecl)
        | FSharpImplementationFileDeclaration.InitAction _action -> ()
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (value = mfv ; body = body) ->
            if mfv.IsFunction then
                let asyncOrTask =
                    match body.Type.TypeDefinition.TryGetFullName () with
                    | Some fullName ->
                        if fullName.StartsWith ("Microsoft.FSharp.Control.FSharpAsync", StringComparison.Ordinal) then
                            Some "async"
                        elif fullName.StartsWith ("System.Threading.Tasks.Task", StringComparison.Ordinal) then
                            Some "task"
                        else
                            None

                    | None -> None

                match asyncOrTask with
                | Some ce ->
                    let rangesToWarnAbout = getUseRanges uses body |> Array.map (fun r -> (ce, r))
                    state.AddRange rangesToWarnAbout
                | None -> ()

    traverseDecl decl

    [
        for ce, range in state do
            {
                Type = "DisposedBeforeAsyncRun analyzer"
                Message = $"Object is disposed before returned %s{ce} is run"
                Code = Code
                Severity = Warning
                Range = range
                Fixes = []
            }
    ]

let analyze (sourceText : ISourceText) parseTree (typedTree : FSharpImplementationFileContents) =
    let uses = collectUses parseTree sourceText
    typedTree.Declarations |> List.collect (analyzeAsyncOrTaskReturningDecls uses)

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
        async {
            return
                ctx.TypedTree
                |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let disposedBeforeAsyncRunEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) ->
        async {
            return
                ctx.TypedTree
                |> Option.map (analyze ctx.SourceText ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }
