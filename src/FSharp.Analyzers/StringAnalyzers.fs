namespace GR.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

module StringAnalyzers =
    [<Literal>]
    let EndsWithCode = "GRA-001"

    let (|SingleNameInSynLongIdent|_|) name (lid : SynLongIdent) =
        match lid with
        | SynLongIdent (id = [ ident ]) when ident.idText = name -> Some ident.idRange
        | _ -> None

    let (|SynLongIdentEndsWith|_|) name (lid : SynLongIdent) =
        List.tryLast lid.LongIdent
        |> Option.bind (fun ident -> if ident.idText = name then Some ident.idRange else None)

    let rec (|SingleStringArgumentExpr|_|) =
        function
        | SynExpr.Paren (expr = SingleStringArgumentExpr)
        // ""
        | SynExpr.Const (constant = SynConst.String _)
        // a.b
        | SynExpr.LongIdent _
        // a
        | SynExpr.Ident _ -> Some ()
        | _ -> None

    let findAllInvocations
        (parameterPredicate : SynExpr -> bool)
        (functionName : string)
        (ast : ParsedInput)
        : range list
        =
        let collector = ResizeArray<range> ()

        let walker =
            { new SyntaxCollectorBase() with
                override _.WalkExpr (expr : SynExpr) =
                    match expr with
                    // "".FunctionName arg
                    | SynExpr.App (
                        isInfix = false
                        funcExpr = SynExpr.DotGet (longDotId = SingleNameInSynLongIdent functionName mFunctionName)
                        argExpr = argExpr)

                    // w.FunctionName arg
                    | SynExpr.App (
                        funcExpr = SynExpr.LongIdent (longDotId = SynLongIdentEndsWith functionName mFunctionName)
                        argExpr = argExpr) when parameterPredicate argExpr -> collector.Add mFunctionName

                    | _ -> ()
            }

        walkAst walker ast

        collector |> Seq.toList

    let invalidStringFunctionUseAnalyzer
        functionName
        code
        message
        severity
        (sourceText : ISourceText)
        (untypedTree : ParsedInput)
        (checkFileResults : FSharpCheckFileResults)
        (parameterPredicate : SynExpr -> bool)
        =
        async {
            let invocations = findAllInvocations parameterPredicate functionName untypedTree

            return
                invocations
                |> List.choose (fun mEndsWith ->
                    let lineText = sourceText.GetLineString (mEndsWith.EndLine - 1)

                    let symbolUseOpt =
                        checkFileResults.GetSymbolUseAtLocation (
                            mEndsWith.EndLine,
                            mEndsWith.EndColumn,
                            lineText,
                            [ "EndsWith" ]
                        )

                    symbolUseOpt
                    |> Option.bind (fun symbolUse ->
                        match symbolUse.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as mfv ->
                            if mfv.Assembly.SimpleName <> "netstandard" then
                                None
                            elif mfv.CurriedParameterGroups.Count <> 1 then
                                None
                            elif mfv.CurriedParameterGroups.[0].Count <> 1 then
                                None
                            elif
                                mfv.CurriedParameterGroups.[0].[0].Type.ErasedType.BasicQualifiedName
                                <> "System.String"
                            then
                                None
                            else
                                Some mEndsWith
                        | _ -> None
                    )
                )
                |> List.map (fun mFunctionName ->
                    {
                        Type = $"String.{functionName} analyzer"
                        Message = message
                        Code = code
                        Severity = severity
                        Range = mFunctionName
                        Fixes = []
                    }
                )
        }

    [<CliAnalyzer>]
    let endsWithAnalyzer (ctx : CliContext) : Async<Message list> =
        invalidStringFunctionUseAnalyzer
            "EndsWith"
            EndsWithCode
            "The usage of String.EndsWith with a single string argument is discouraged. Signal your intention explicitly by calling an overload."
            Warning
            ctx.SourceText
            ctx.ParseFileResults.ParseTree
            ctx.CheckFileResults
            (function
            | SingleStringArgumentExpr _ -> true
            | _ -> false
            )
