module GR.FSharp.Analyzers.TypeAnnotateStringFunctionAnalyzer

open System.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type ISourceText with

    member x.GetContentAt (range : range) : string =
        let startLine = range.StartLine - 1
        let line = x.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring (range.StartColumn, length)
        else
            let firstLineContent = line.Substring range.StartColumn
            let sb = StringBuilder().AppendLine firstLineContent

            (sb, [ range.StartLine .. range.EndLine - 2 ])
            ||> List.fold (fun sb lineNumber -> sb.AppendLine (x.GetLineString lineNumber))
            |> fun sb ->
                let lastLine = x.GetLineString (range.EndLine - 1)

                sb.Append(lastLine.Substring (0, range.EndColumn)).ToString ()

[<RequireQualifiedAccess>]
type StringApplicationResult =
    | NoTypeArgument
    | TypeArgument
    | Unsure of string

let (|StringFunctionExpr|_|) =
    function
    | SynExpr.Ident (ident = ident) when ident.idText = "string" -> Some ()
    | SynExpr.LongIdent (longDotId = SynLongIdent (id = lid)) ->
        List.tryLast lid
        |> Option.bind (fun ident -> if ident.idText = "string" then Some () else None)
    | _ -> None

[<CliAnalyzer("TypeAnnotateStringFunctionAnalyzer",
              "Checks if the `string` function call is type annotated.",
              "https://g-research.github.io/fsharp-analyzers/analyzers/TypeAnnotateStringFunctionAnalyzer.html")>]
let typeAnnotateStringFunctionsAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {
            let messages = ResizeArray<Message> ()

            let tryFindSynExprApp (m : range) =
                let visitor =
                    { new SyntaxVisitorBase<StringApplicationResult>() with
                        override x.VisitExpr (path, traverseSynExpr, defaultTraverse, synExpr) =
                            if not (Range.equals synExpr.Range m) then
                                defaultTraverse synExpr
                            else
                                match synExpr with
                                // in this case expression was probably piped into the string function.
                                | StringFunctionExpr ->
                                    match path with
                                    | SyntaxNode.SynExpr (SynExpr.TypeApp _) :: _ ->
                                        Some StringApplicationResult.TypeArgument
                                    | _ -> Some StringApplicationResult.NoTypeArgument

                                | SynExpr.App (funcExpr = StringFunctionExpr) ->
                                    Some StringApplicationResult.NoTypeArgument

                                | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = StringFunctionExpr)) ->
                                    Some StringApplicationResult.TypeArgument
                                | e ->
                                    let source = ctx.SourceText.GetContentAt e.Range
                                    Some (StringApplicationResult.Unsure source)
                    }

                SyntaxTraversal.Traverse (m.Start, ctx.ParseFileResults.ParseTree, visitor)

            let tastCollector =
                { new TypedTreeCollectorBase() with
                    override x.WalkCall
                        _
                        (mfv : FSharpMemberOrFunctionOrValue)
                        objExprTypeArgs
                        memberOrFuncTypeArgs
                        (args : FSharpExpr list)
                        (m : range)
                        =
                        if mfv.FullName = "Microsoft.FSharp.Core.Operators.string" && args.Length = 1 then
                            match tryFindSynExprApp m with
                            | Some (StringApplicationResult.Unsure sourceCode) ->
#if DEBUG
                                printfn $"Could map %A{m} to a string application, source:\n%s{sourceCode}"
#else
                                ignore sourceCode
#endif
                            | Some StringApplicationResult.NoTypeArgument ->
                                messages.Add
                                    {
                                        Type = "TypeAnnotateStringFunctionsAnalyzer"
                                        Message = "Please annotate your type when using the `string` function."
                                        Code = "GRA-TYPE-ANNOTATE-001"
                                        Severity = Severity.Warning
                                        Range = m
                                        Fixes = []
                                    }
                            | Some StringApplicationResult.TypeArgument -> ()
                            | None ->
#if DEBUG
                                printfn $"Could not find application for %A{m}"
#else
                                ()
#endif
                }

            match ctx.TypedTree with
            | None -> return []
            | Some typedTree ->
                walkTast tastCollector typedTree
                return Seq.toList messages
        }
