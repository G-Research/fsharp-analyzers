namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK
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

    let rec (|SingleArgumentExpr|_|) =
        function
        | SynExpr.Paren (expr = SingleArgumentExpr)
        // ""
        | SynExpr.Const (constant = SynConst.String _)
        // a.b
        | SynExpr.LongIdent _
        // a
        | SynExpr.Ident _ -> Some ()
        | _ -> None

    let findAllInvocations (functionName : string) (ast : ParsedInput) : range list =
        let collector = ResizeArray<range> ()

        let walker =
            { new SyntaxCollectorBase() with
                override _.WalkExpr (expr : SynExpr) =
                    match expr with
                    // "".EndsWith("a")
                    | SynExpr.App (
                        isInfix = false
                        funcExpr = SynExpr.DotGet (longDotId = SingleNameInSynLongIdent functionName mFunctionName)
                        argExpr = SingleArgumentExpr)

                    // w.EndsWith("a")
                    | SynExpr.App (
                        funcExpr = SynExpr.LongIdent (longDotId = SynLongIdentEndsWith functionName mFunctionName)
                        argExpr = SingleArgumentExpr) -> collector.Add mFunctionName

                    | _ -> ()
            }

        walkAst walker ast

        collector |> Seq.toList

    [<CliAnalyzer>]
    let endsWithAnalyzer (ctx : CliContext) : Async<Message list> =
        async {
            let invocations = findAllInvocations "EndsWith" ctx.ParseFileResults.ParseTree

            return
                invocations
                |> List.choose (fun mEndsWith ->
                    let lineText = ctx.SourceText.GetLineString (mEndsWith.EndLine - 1)

                    let symbolUseOpt =
                        ctx.CheckFileResults.GetSymbolUseAtLocation (
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
                |> List.map (fun mEndsWith ->
                    {
                        Type = "String.EndsWith analyzer"
                        Message =
                            "The usage of String.EndsWith with a single string argument is discouraged. Signal your intention explicitly by calling an overload."
                        Code = EndsWithCode
                        Severity = Warning
                        Range = mEndsWith
                        Fixes = []
                    }
                )
        }
