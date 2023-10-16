module GR.FSharp.Analyzers.PartialAppAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open Microsoft.FSharp.Core

[<Literal>]
let Code = "GRA-PARTAPP-001"

type Handler = string * FSharp.Compiler.Text.range * int -> unit

let collectExprFromMatchClauses (clauses : SynMatchClause list) =
    clauses
    |> List.map (fun (SynMatchClause (whenExpr = whenExpr ; resultExpr = resultExpr)) ->
        [
            if Option.isSome whenExpr then
                whenExpr.Value
            else
                ()
                resultExpr
        ]
    )
    |> List.concat

let rec visitApp (handler : Handler) (depth : int) (expr : SynExpr) =
    match expr with
    | SynExpr.App (funcExpr = SynExpr.Paren (expr = expr) ; argExpr = argExpr) ->
        visitApp handler depth expr
        visitExpr handler argExpr
    | SynExpr.App (funcExpr = SynExpr.Ident i) -> handler (i.idText, i.idRange, 1 + depth)
    | SynExpr.App (funcExpr = SynExpr.LongIdent (longDotId = longDotId) ; argExpr = argExpr) ->
        longDotId.IdentsWithTrivia
        |> Seq.tryLast
        |> Option.iter (fun (SynIdent.SynIdent (ident = ident)) -> handler (ident.idText, longDotId.Range, 1 + depth))

        visitApp handler depth argExpr
    | SynExpr.App (funcExpr = SynExpr.App _ as funcExpr ; argExpr = argExpr) ->
        visitApp handler (1 + depth) funcExpr
        visitApp handler depth argExpr
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.Ident i)) -> handler (i.idText, i.idRange, 1 + depth)
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.LongIdent (longDotId = longDotId))) ->
        longDotId.IdentsWithTrivia
        |> Seq.tryLast
        |> Option.iter (fun (SynIdent.SynIdent (ident = ident)) -> handler (ident.idText, longDotId.Range, 1 + depth))
    | SynExpr.IfThenElse (ifExpr = ifExpr ; thenExpr = thenExpr ; elseExpr = elseExpr) ->
        visitApp handler depth ifExpr
        visitApp handler depth thenExpr
        Option.iter (visitApp handler depth) elseExpr
    | SynExpr.Match (expr = expr ; clauses = clauses) ->
        let matchExprs = collectExprFromMatchClauses clauses
        visitApp handler depth expr
        List.iter (visitApp handler depth) matchExprs
    | SynExpr.MatchLambda (matchClauses = matchClauses) ->
        let matchExprs = collectExprFromMatchClauses matchClauses
        List.iter (visitApp handler depth) matchExprs
    | SynExpr.App (funcExpr = SynExpr.DotGet _) -> ()
    | SynExpr.App (funcExpr = SynExpr.DotIndexedGet _) -> ()
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.DotIndexedGet _)) -> ()
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.DotGet _)) -> ()
    | SynExpr.Const _
    | SynExpr.LongIdent _ -> ()

    | _ ->
        // printfn $"visitApp: not supported yet {expr}"
        ()

and visitMatchClause (handler : Handler) (SynMatchClause (resultExpr = resultExpr ; whenExpr = whenExpr)) =
    visitExpr handler resultExpr
    Option.iter (visitExpr handler) whenExpr

and visitRecordField (handler : Handler) (SynExprRecordField (expr = expr)) = Option.iter (visitExpr handler) expr

and visitAndBang (handler : Handler) (SynExprAndBang (body = body)) = visitExpr handler body

and visitInterpolatedStringParts (handler : Handler) (part : SynInterpolatedStringPart) =
    match part with
    | SynInterpolatedStringPart.FillExpr (fillExpr = fillExpr) -> visitExpr handler fillExpr
    | _ -> ()

and visitExpr (handler : Handler) (expr : SynExpr) =
    match expr with
    | SynExpr.App _ as e -> visitApp handler 0 e
    | SynExpr.Lambda (body = body) -> visitExpr handler body
    | SynExpr.LetOrUse (bindings = bindings ; body = body) ->
        List.iter (visitBinding handler) bindings
        visitExpr handler body
    | SynExpr.Do (expr = expr) -> visitExpr handler expr
    | SynExpr.Sequential (expr1 = expr1 ; expr2 = expr2) ->
        visitExpr handler expr1
        visitExpr handler expr2
    | SynExpr.Typed (expr = expr) -> visitExpr handler expr
    | SynExpr.Match (expr = expr ; clauses = clauses) ->
        visitExpr handler expr
        clauses |> List.iter (visitMatchClause handler)
    | SynExpr.Assert (expr = expr) -> visitExpr handler expr
    | SynExpr.Downcast (expr = expr) -> visitExpr handler expr
    | SynExpr.Dynamic (funcExpr = funcExpr ; argExpr = argExpr) ->
        visitExpr handler funcExpr
        visitExpr handler argExpr
    | SynExpr.Fixed (expr = expr) -> visitExpr handler expr
    | SynExpr.For (identBody = identBody ; toBody = synExpr ; doBody = doBody) ->
        visitExpr handler identBody
        visitExpr handler synExpr
        visitExpr handler doBody
    | SynExpr.Lazy (expr = expr) -> visitExpr handler expr
    | SynExpr.New (expr = expr) -> visitExpr handler expr
    | SynExpr.Paren (expr = expr) -> visitExpr handler expr
    | SynExpr.Quote (operator = operator ; quotedExpr = quotedExpr) ->
        visitExpr handler operator
        visitExpr handler quotedExpr
    | SynExpr.Record (baseInfo = baseInfo ; copyInfo = copyInfo ; recordFields = recordFields) ->
        match baseInfo with
        | Some (_, expr, _, _, _) -> visitExpr handler expr
        | None -> ()

        match copyInfo with
        | Some (expr, _) -> visitExpr handler expr
        | None -> ()

        recordFields |> List.iter (visitRecordField handler)
    | SynExpr.Set (targetExpr = targetExpr ; rhsExpr = rhsExpr) ->
        visitExpr handler targetExpr
        visitExpr handler rhsExpr
    | SynExpr.Tuple (exprs = exprs) -> List.iter (visitExpr handler) exprs
    | SynExpr.Upcast (expr = expr) -> visitExpr handler expr
    | SynExpr.While (whileExpr = whileExpr ; doExpr = doExpr) ->
        visitExpr handler whileExpr
        visitExpr handler doExpr
    | SynExpr.AddressOf (expr = expr) -> visitExpr handler expr
    | SynExpr.AnonRecd (copyInfo = copyInfo ; recordFields = recordFields) ->
        match copyInfo with
        | Some (expr, _) -> visitExpr handler expr
        | None -> ()

        recordFields |> List.iter (fun (_, _, expr) -> visitExpr handler expr)
    | SynExpr.ComputationExpr (expr = expr) -> visitExpr handler expr
    | SynExpr.DebugPoint (innerExpr = innerExpr) -> visitExpr handler innerExpr
    | SynExpr.DoBang (expr = expr) -> visitExpr handler expr
    | SynExpr.DotGet (expr = expr) -> visitExpr handler expr
    | SynExpr.DotSet (targetExpr = targetExpr ; rhsExpr = rhsExpr) ->
        visitExpr handler targetExpr
        visitExpr handler rhsExpr
    | SynExpr.ForEach (enumExpr = enumExpr ; bodyExpr = bodyExpr) ->
        visitExpr handler enumExpr
        visitExpr handler bodyExpr
    | SynExpr.ArrayOrList (exprs = exprs) -> List.iter (visitExpr handler) exprs
    | SynExpr.ObjExpr (argOptions = argOptions ; bindings = bindings ; members = members ; extraImpls = extraImpls) ->
        match argOptions with
        | Some (expr, _) -> visitExpr handler expr
        | None -> ()

        List.iter (visitBinding handler) bindings
        List.iter (visitMemberDefn handler) members
        List.iter (visitInterfaceImpl handler) extraImpls
    | SynExpr.ArrayOrListComputed (expr = expr) -> visitExpr handler expr
    | SynExpr.IndexRange (expr1 = expr1 ; expr2 = expr2) ->
        Option.iter (visitExpr handler) expr1
        Option.iter (visitExpr handler) expr2
    | SynExpr.IndexFromEnd (expr = expr) -> visitExpr handler expr
    | SynExpr.MatchLambda (matchClauses = matchClauses) -> List.iter (visitMatchClause handler) matchClauses
    | SynExpr.TypeApp (expr = expr) -> visitExpr handler expr
    | SynExpr.TryWith (tryExpr = tryExpr ; withCases = withCases) ->
        visitExpr handler tryExpr
        List.iter (visitMatchClause handler) withCases
    | SynExpr.TryFinally (tryExpr = tryExpr ; finallyExpr = finallyExpr) ->
        visitExpr handler tryExpr
        visitExpr handler finallyExpr
    | SynExpr.IfThenElse (ifExpr = ifExpr ; thenExpr = thenExpr ; elseExpr = elseExpr) ->
        visitExpr handler ifExpr
        visitExpr handler thenExpr
        Option.iter (visitExpr handler) elseExpr
    | SynExpr.LongIdentSet (expr = expr) -> visitExpr handler expr
    | SynExpr.DotIndexedGet (objectExpr = objectExpr ; indexArgs = indexArgs) ->
        visitExpr handler objectExpr
        visitExpr handler indexArgs
    | SynExpr.DotIndexedSet (objectExpr = objectExpr ; indexArgs = indexArgs ; valueExpr = valueExpr) ->
        visitExpr handler objectExpr
        visitExpr handler indexArgs
        visitExpr handler valueExpr
    | SynExpr.NamedIndexedPropertySet (expr1 = expr1 ; expr2 = expr2) ->
        visitExpr handler expr1
        visitExpr handler expr2
    | SynExpr.DotNamedIndexedPropertySet (targetExpr = targetExpr ; argExpr = argExpr ; rhsExpr = rhsExpr) ->
        visitExpr handler targetExpr
        visitExpr handler argExpr
        visitExpr handler rhsExpr
    | SynExpr.TypeTest (expr = expr) -> visitExpr handler expr
    | SynExpr.InferredUpcast (expr = expr) -> visitExpr handler expr
    | SynExpr.InferredDowncast (expr = expr) -> visitExpr handler expr
    | SynExpr.TraitCall (argExpr = argExpr) -> visitExpr handler argExpr
    | SynExpr.JoinIn (lhsExpr = lhsExpr ; rhsExpr = rhsExpr) ->
        visitExpr handler lhsExpr
        visitExpr handler rhsExpr
    | SynExpr.SequentialOrImplicitYield (expr1 = expr1 ; expr2 = expr2 ; ifNotStmt = ifNotStmt) ->
        visitExpr handler expr1
        visitExpr handler expr2
        visitExpr handler ifNotStmt
    | SynExpr.YieldOrReturn (expr = expr) -> visitExpr handler expr
    | SynExpr.YieldOrReturnFrom (expr = expr) -> visitExpr handler expr
    | SynExpr.LetOrUseBang (rhs = rhs ; andBangs = andBangs ; body = body) ->
        visitExpr handler rhs
        List.iter (visitAndBang handler) andBangs
        visitExpr handler body
    | SynExpr.MatchBang (expr = expr ; clauses = clauses) ->
        visitExpr handler expr
        List.iter (visitMatchClause handler) clauses
    | SynExpr.LibraryOnlyILAssembly (args = args) -> List.iter (visitExpr handler) args
    | SynExpr.LibraryOnlyStaticOptimization (expr = expr ; optimizedExpr = optimizedExpr) ->
        visitExpr handler expr
        visitExpr handler optimizedExpr
    | SynExpr.LibraryOnlyUnionCaseFieldGet (expr = expr) -> visitExpr handler expr
    | SynExpr.LibraryOnlyUnionCaseFieldSet (expr = expr ; rhsExpr = rhsExpr) ->
        visitExpr handler expr
        visitExpr handler rhsExpr
    | SynExpr.InterpolatedString (contents = contents) -> List.iter (visitInterpolatedStringParts handler) contents
    | _ -> ()

and visitBinding (handler : Handler) (SynBinding (expr = expr)) = visitExpr handler expr

and visitInterfaceImpl (handler : Handler) (SynInterfaceImpl (bindings = bindings ; members = members)) =
    List.iter (visitBinding handler) bindings
    List.iter (visitMemberDefn handler) members

and visitMemberDefn (handler : Handler) (memberDefn : SynMemberDefn) =
    match memberDefn with
    | SynMemberDefn.LetBindings (bindings = bindings) -> List.iter (visitBinding handler) bindings
    | SynMemberDefn.Member (memberDefn = memberDefn) -> visitBinding handler memberDefn
    | SynMemberDefn.GetSetMember (memberDefnForGet = memberDefnForGet ; memberDefnForSet = memberDefnForSet) ->
        Option.iter (visitBinding handler) memberDefnForGet
        Option.iter (visitBinding handler) memberDefnForSet
    | SynMemberDefn.Interface (members = members) -> Option.iter (List.iter (visitMemberDefn handler)) members
    | SynMemberDefn.NestedType (typeDefn = typeDefn) -> visitTypeDefn handler typeDefn
    | SynMemberDefn.AutoProperty (synExpr = synExpr) -> visitExpr handler synExpr
    | _ -> ()

and visitTypeDefn (handler : Handler) (synTypeDefn : SynTypeDefn) =
    match synTypeDefn with
    | SynTypeDefn.SynTypeDefn (typeRepr = typeRepr ; members = members) ->
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel (members = members) -> List.iter (visitMemberDefn handler) members
        | SynTypeDefnRepr.Simple _ -> List.iter (visitMemberDefn handler) members
        | _ -> ()

and visitModuleDecl (handler : Handler) (decl : SynModuleDecl) =
    match decl with
    | SynModuleDecl.NestedModule (decls = decls) -> List.iter (visitModuleDecl handler) decls
    | SynModuleDecl.Let (bindings = bindings) -> List.iter (visitBinding handler) bindings
    | SynModuleDecl.Expr (expr = expr) -> visitExpr handler expr
    | SynModuleDecl.Types (typeDefns = typeDefns) -> List.iter (visitTypeDefn handler) typeDefns
    | SynModuleDecl.Exception (exnDefn = SynExceptionDefn (members = members)) ->
        List.iter (visitMemberDefn handler) members
    | _ -> ()

and visitModuleDecls (handler : Handler) (decls : SynModuleDecl list) =
    List.iter (visitModuleDecl handler) decls

and visitModuleOrNamespace (handler : Handler) (SynModuleOrNamespace (decls = decls) : SynModuleOrNamespace) =
    visitModuleDecls handler decls

let tryGetParameterCount (symbolUses : FSharpSymbolUse seq) r =
    symbolUses
    |> Seq.tryFind (fun s -> s.Range = r)
    |> Option.map (fun s ->
        match s.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv -> Some mfv.CurriedParameterGroups.Count
        | _ -> None
    )
    |> Option.flatten

let analyze parseTree (checkFileResults : FSharpCheckFileResults) =
    let state = ResizeArray<string * FSharp.Compiler.Text.range * int> ()
    let handler : Handler = fun (ident, r, args) -> state.Add (ident, r, args)

    match parseTree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (contents = contents)) ->
        contents |> List.iter (visitModuleOrNamespace handler)
    | _ -> ()

    let msgs =
        seq {
            for app in state do
                let _ident, range, providedArgsCount = app

                let parameterCount =
                    let symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile ()
                    tryGetParameterCount symbolUses range

                match parameterCount with
                | Some paramsCount ->
                    if providedArgsCount < paramsCount then // use LESS, not NOT EQUAL because of CEs, printf, etc. take more than paramsCount

                        let msg =
                            {
                                Type = "Partial Application Analyzer"
                                Message = $"Partial application should not be used."
                                Code = Code
                                Severity = Warning
                                Range = range
                                Fixes = []
                            }

                        yield msg
                | None -> ()
        }
        |> Seq.toList

    msgs

[<CliAnalyzer("PartialAppAnalyzer", "Warns when partial application is being used.")>]
let partialAppCliAnalyzer : Analyzer<CliContext> =
    fun context -> async { return analyze context.ParseFileResults.ParseTree context.CheckFileResults }

[<EditorAnalyzer "PartialAppAnalyzer">]
let partialAppEditorAnalyzer : Analyzer<EditorContext> =
    fun context ->
        async {
            match context.CheckFileResults with
            | Some checkFileResults -> return analyze context.ParseFileResults.ParseTree checkFileResults
            | None -> return []
        }
