module GR.FSharp.Analyzers.PartialAppAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open Microsoft.FSharp.Core

[<Literal>]
let Code = "GRA-PARTAPP-001"

type AppHandler = string * FSharp.Compiler.Text.range * int -> unit

type PipeHandler = FSharp.Compiler.Text.range * int -> unit

type Handlers =
    {
        AppHandler : AppHandler
        PipeHandler : PipeHandler
    }

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

let rec (|AppOfIdent|_|) (expr : SynExpr) =
    match expr with
    | SynExpr.App (funcExpr = SynExpr.Ident ident) -> Some ident
    | SynExpr.App (funcExpr = SynExpr.App _ as a) ->
        match a with
        | AppOfIdent i -> Some i
        | _ -> None
    | _ -> None

let rec (|AppOfLongIdent|_|) (expr : SynExpr) =
    match expr with
    | SynExpr.App (funcExpr = SynExpr.LongIdent (longDotId = ident)) -> Some ident
    | SynExpr.App (funcExpr = SynExpr.App _ as a) ->
        match a with
        | AppOfLongIdent i -> Some i
        | _ -> None
    | _ -> None

let collectPipedArgs (pipeHandler : PipeHandler) (funcExpr : SynExpr) (argExpr : SynExpr) =
    match funcExpr with
    | SynExpr.App (funcExpr = SynExpr.LongIdent (longDotId = longDotId) ; argExpr = argExpr2) ->
        longDotId.LongIdent
        |> List.tryHead
        |> Option.iter (fun i ->
            let pipedArgs, target =
                match i.idText with
                | "op_PipeRight" -> 1, Some argExpr
                | "op_PipeRight2" -> 2, Some argExpr
                | "op_PipeRight3" -> 3, Some argExpr
                // not needed for op_PipeLeft
                | "op_PipeLeft2" -> 2, Some argExpr2
                | "op_PipeLeft3" -> 3, Some argExpr2
                | _ -> 0, None

            if pipedArgs > 0 then
                target
                |> Option.iter (
                    function
                    | AppOfIdent i -> pipeHandler (i.idRange, pipedArgs)
                    | AppOfLongIdent i -> pipeHandler (i.Range, pipedArgs)
                    | _ -> ()
                )
        )
    | _ -> ()

let rec visitApp (handlers : Handlers) (depth : int) (expr : SynExpr) =
    match expr with
    | SynExpr.App (funcExpr = SynExpr.Paren (expr = expr) ; argExpr = argExpr) ->
        visitApp handlers depth expr
        visitExpr handlers argExpr
    | SynExpr.App (funcExpr = SynExpr.Ident i) -> handlers.AppHandler (i.idText, i.idRange, 1 + depth)
    | SynExpr.App (funcExpr = SynExpr.LongIdent (longDotId = longDotId) ; argExpr = argExpr) ->
        longDotId.IdentsWithTrivia
        |> List.tryLast
        |> Option.iter (fun (SynIdent.SynIdent (ident = ident)) ->
            handlers.AppHandler (ident.idText, longDotId.Range, 1 + depth)
        )

        visitApp handlers depth argExpr
    | SynExpr.App (funcExpr = SynExpr.App _ as funcExpr ; argExpr = argExpr) ->
        collectPipedArgs handlers.PipeHandler funcExpr argExpr
        visitApp handlers (1 + depth) funcExpr
        visitApp handlers depth argExpr
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.Ident i)) ->
        handlers.AppHandler (i.idText, i.idRange, 1 + depth)
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.LongIdent (longDotId = longDotId))) ->
        longDotId.IdentsWithTrivia
        |> List.tryLast
        |> Option.iter (fun (SynIdent.SynIdent (ident = ident)) ->
            handlers.AppHandler (ident.idText, longDotId.Range, 1 + depth)
        )
    | SynExpr.IfThenElse (ifExpr = ifExpr ; thenExpr = thenExpr ; elseExpr = elseExpr) ->
        visitApp handlers depth ifExpr
        visitApp handlers depth thenExpr
        Option.iter (visitApp handlers depth) elseExpr
    | SynExpr.Paren (expr = synExpr) -> visitExpr handlers synExpr
    | SynExpr.Match (expr = expr ; clauses = clauses) ->
        let matchExprs = collectExprFromMatchClauses clauses
        visitApp handlers depth expr
        List.iter (visitApp handlers depth) matchExprs
    | SynExpr.MatchLambda (matchClauses = matchClauses) ->
        let matchExprs = collectExprFromMatchClauses matchClauses
        List.iter (visitApp handlers depth) matchExprs
    | SynExpr.App (funcExpr = SynExpr.DotGet _) -> ()
    | SynExpr.App (funcExpr = SynExpr.DotIndexedGet _) -> ()
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.DotIndexedGet _)) -> ()
    | SynExpr.App (funcExpr = SynExpr.TypeApp (expr = SynExpr.DotGet _)) -> ()
    | SynExpr.Const _
    | SynExpr.LongIdent _ -> ()
    | _ -> ()

and visitMatchClause (handlers : Handlers) (SynMatchClause (resultExpr = resultExpr ; whenExpr = whenExpr)) =
    visitExpr handlers resultExpr
    Option.iter (visitExpr handlers) whenExpr

and visitRecordField (handlers : Handlers) (SynExprRecordField (expr = expr)) = Option.iter (visitExpr handlers) expr

and visitAndBang (handlers : Handlers) (SynExprAndBang (body = body)) = visitExpr handlers body

and visitInterpolatedStringParts (handlers : Handlers) (part : SynInterpolatedStringPart) =
    match part with
    | SynInterpolatedStringPart.FillExpr (fillExpr = fillExpr) -> visitExpr handlers fillExpr
    | _ -> ()

and visitExpr (handlers : Handlers) (expr : SynExpr) =
    match expr with
    | SynExpr.App _ as e -> visitApp handlers 0 e
    | SynExpr.Lambda (body = body) -> visitExpr handlers body
    | SynExpr.LetOrUse (bindings = bindings ; body = body) ->
        List.iter (visitBinding handlers) bindings
        visitExpr handlers body
    | SynExpr.Do (expr = expr) -> visitExpr handlers expr
    | SynExpr.Sequential (expr1 = expr1 ; expr2 = expr2) ->
        visitExpr handlers expr1
        visitExpr handlers expr2
    | SynExpr.Typed (expr = expr) -> visitExpr handlers expr
    | SynExpr.Match (expr = expr ; clauses = clauses) ->
        visitExpr handlers expr
        clauses |> List.iter (visitMatchClause handlers)
    | SynExpr.Assert (expr = expr) -> visitExpr handlers expr
    | SynExpr.Downcast (expr = expr) -> visitExpr handlers expr
    | SynExpr.Dynamic (funcExpr = funcExpr ; argExpr = argExpr) ->
        visitExpr handlers funcExpr
        visitExpr handlers argExpr
    | SynExpr.Fixed (expr = expr) -> visitExpr handlers expr
    | SynExpr.For (identBody = identBody ; toBody = synExpr ; doBody = doBody) ->
        visitExpr handlers identBody
        visitExpr handlers synExpr
        visitExpr handlers doBody
    | SynExpr.Lazy (expr = expr) -> visitExpr handlers expr
    | SynExpr.New (expr = expr) -> visitExpr handlers expr
    | SynExpr.Paren (expr = expr) -> visitExpr handlers expr
    | SynExpr.Quote (operator = operator ; quotedExpr = quotedExpr) ->
        visitExpr handlers operator
        visitExpr handlers quotedExpr
    | SynExpr.Record (baseInfo = baseInfo ; copyInfo = copyInfo ; recordFields = recordFields) ->
        match baseInfo with
        | Some (_, expr, _, _, _) -> visitExpr handlers expr
        | None -> ()

        match copyInfo with
        | Some (expr, _) -> visitExpr handlers expr
        | None -> ()

        recordFields |> List.iter (visitRecordField handlers)
    | SynExpr.Set (targetExpr = targetExpr ; rhsExpr = rhsExpr) ->
        visitExpr handlers targetExpr
        visitExpr handlers rhsExpr
    | SynExpr.Tuple (exprs = exprs) -> List.iter (visitExpr handlers) exprs
    | SynExpr.Upcast (expr = expr) -> visitExpr handlers expr
    | SynExpr.While (whileExpr = whileExpr ; doExpr = doExpr) ->
        visitExpr handlers whileExpr
        visitExpr handlers doExpr
    | SynExpr.AddressOf (expr = expr) -> visitExpr handlers expr
    | SynExpr.AnonRecd (copyInfo = copyInfo ; recordFields = recordFields) ->
        match copyInfo with
        | Some (expr, _) -> visitExpr handlers expr
        | None -> ()

        recordFields |> List.iter (fun (_, _, expr) -> visitExpr handlers expr)
    | SynExpr.ComputationExpr (expr = expr) -> visitExpr handlers expr
    | SynExpr.DebugPoint (innerExpr = innerExpr) -> visitExpr handlers innerExpr
    | SynExpr.DoBang (expr = expr) -> visitExpr handlers expr
    | SynExpr.DotGet (expr = expr) -> visitExpr handlers expr
    | SynExpr.DotSet (targetExpr = targetExpr ; rhsExpr = rhsExpr) ->
        visitExpr handlers targetExpr
        visitExpr handlers rhsExpr
    | SynExpr.ForEach (enumExpr = enumExpr ; bodyExpr = bodyExpr) ->
        visitExpr handlers enumExpr
        visitExpr handlers bodyExpr
    | SynExpr.ArrayOrList (exprs = exprs) -> List.iter (visitExpr handlers) exprs
    | SynExpr.ObjExpr (argOptions = argOptions ; bindings = bindings ; members = members ; extraImpls = extraImpls) ->
        match argOptions with
        | Some (expr, _) -> visitExpr handlers expr
        | None -> ()

        List.iter (visitBinding handlers) bindings
        List.iter (visitMemberDefn handlers) members
        List.iter (visitInterfaceImpl handlers) extraImpls
    | SynExpr.ArrayOrListComputed (expr = expr) -> visitExpr handlers expr
    | SynExpr.IndexRange (expr1 = expr1 ; expr2 = expr2) ->
        Option.iter (visitExpr handlers) expr1
        Option.iter (visitExpr handlers) expr2
    | SynExpr.IndexFromEnd (expr = expr) -> visitExpr handlers expr
    | SynExpr.MatchLambda (matchClauses = matchClauses) -> List.iter (visitMatchClause handlers) matchClauses
    | SynExpr.TypeApp (expr = expr) -> visitExpr handlers expr
    | SynExpr.TryWith (tryExpr = tryExpr ; withCases = withCases) ->
        visitExpr handlers tryExpr
        List.iter (visitMatchClause handlers) withCases
    | SynExpr.TryFinally (tryExpr = tryExpr ; finallyExpr = finallyExpr) ->
        visitExpr handlers tryExpr
        visitExpr handlers finallyExpr
    | SynExpr.IfThenElse (ifExpr = ifExpr ; thenExpr = thenExpr ; elseExpr = elseExpr) ->
        visitExpr handlers ifExpr
        visitExpr handlers thenExpr
        Option.iter (visitExpr handlers) elseExpr
    | SynExpr.LongIdentSet (expr = expr) -> visitExpr handlers expr
    | SynExpr.DotIndexedGet (objectExpr = objectExpr ; indexArgs = indexArgs) ->
        visitExpr handlers objectExpr
        visitExpr handlers indexArgs
    | SynExpr.DotIndexedSet (objectExpr = objectExpr ; indexArgs = indexArgs ; valueExpr = valueExpr) ->
        visitExpr handlers objectExpr
        visitExpr handlers indexArgs
        visitExpr handlers valueExpr
    | SynExpr.NamedIndexedPropertySet (expr1 = expr1 ; expr2 = expr2) ->
        visitExpr handlers expr1
        visitExpr handlers expr2
    | SynExpr.DotNamedIndexedPropertySet (targetExpr = targetExpr ; argExpr = argExpr ; rhsExpr = rhsExpr) ->
        visitExpr handlers targetExpr
        visitExpr handlers argExpr
        visitExpr handlers rhsExpr
    | SynExpr.TypeTest (expr = expr) -> visitExpr handlers expr
    | SynExpr.InferredUpcast (expr = expr) -> visitExpr handlers expr
    | SynExpr.InferredDowncast (expr = expr) -> visitExpr handlers expr
    | SynExpr.TraitCall (argExpr = argExpr) -> visitExpr handlers argExpr
    | SynExpr.JoinIn (lhsExpr = lhsExpr ; rhsExpr = rhsExpr) ->
        visitExpr handlers lhsExpr
        visitExpr handlers rhsExpr
    | SynExpr.SequentialOrImplicitYield (expr1 = expr1 ; expr2 = expr2 ; ifNotStmt = ifNotStmt) ->
        visitExpr handlers expr1
        visitExpr handlers expr2
        visitExpr handlers ifNotStmt
    | SynExpr.YieldOrReturn (expr = expr) -> visitExpr handlers expr
    | SynExpr.YieldOrReturnFrom (expr = expr) -> visitExpr handlers expr
    | SynExpr.LetOrUseBang (rhs = rhs ; andBangs = andBangs ; body = body) ->
        visitExpr handlers rhs
        List.iter (visitAndBang handlers) andBangs
        visitExpr handlers body
    | SynExpr.MatchBang (expr = expr ; clauses = clauses) ->
        visitExpr handlers expr
        List.iter (visitMatchClause handlers) clauses
    | SynExpr.LibraryOnlyILAssembly (args = args) -> List.iter (visitExpr handlers) args
    | SynExpr.LibraryOnlyStaticOptimization (expr = expr ; optimizedExpr = optimizedExpr) ->
        visitExpr handlers expr
        visitExpr handlers optimizedExpr
    | SynExpr.LibraryOnlyUnionCaseFieldGet (expr = expr) -> visitExpr handlers expr
    | SynExpr.LibraryOnlyUnionCaseFieldSet (expr = expr ; rhsExpr = rhsExpr) ->
        visitExpr handlers expr
        visitExpr handlers rhsExpr
    | SynExpr.InterpolatedString (contents = contents) -> List.iter (visitInterpolatedStringParts handlers) contents
    | _ -> ()

and visitBinding (handlers : Handlers) (SynBinding (expr = expr)) = visitExpr handlers expr

and visitInterfaceImpl (handlers : Handlers) (SynInterfaceImpl (bindings = bindings ; members = members)) =
    List.iter (visitBinding handlers) bindings
    List.iter (visitMemberDefn handlers) members

and visitMemberDefn (handlers : Handlers) (memberDefn : SynMemberDefn) =
    match memberDefn with
    | SynMemberDefn.LetBindings (bindings = bindings) -> List.iter (visitBinding handlers) bindings
    | SynMemberDefn.Member (memberDefn = memberDefn) -> visitBinding handlers memberDefn
    | SynMemberDefn.GetSetMember (memberDefnForGet = memberDefnForGet ; memberDefnForSet = memberDefnForSet) ->
        Option.iter (visitBinding handlers) memberDefnForGet
        Option.iter (visitBinding handlers) memberDefnForSet
    | SynMemberDefn.Interface (members = members) -> Option.iter (List.iter (visitMemberDefn handlers)) members
    | SynMemberDefn.NestedType (typeDefn = typeDefn) -> visitTypeDefn handlers typeDefn
    | SynMemberDefn.AutoProperty (synExpr = synExpr) -> visitExpr handlers synExpr
    | _ -> ()

and visitTypeDefn (handlers : Handlers) (synTypeDefn : SynTypeDefn) =
    match synTypeDefn with
    | SynTypeDefn.SynTypeDefn (typeRepr = typeRepr ; members = members) ->
        match typeRepr with
        | SynTypeDefnRepr.ObjectModel (members = members) -> List.iter (visitMemberDefn handlers) members
        | SynTypeDefnRepr.Simple _ -> List.iter (visitMemberDefn handlers) members
        | _ -> ()

and visitModuleDecl (handlers : Handlers) (decl : SynModuleDecl) =
    match decl with
    | SynModuleDecl.NestedModule (decls = decls) -> List.iter (visitModuleDecl handlers) decls
    | SynModuleDecl.Let (bindings = bindings) -> List.iter (visitBinding handlers) bindings
    | SynModuleDecl.Expr (expr = expr) -> visitExpr handlers expr
    | SynModuleDecl.Types (typeDefns = typeDefns) -> List.iter (visitTypeDefn handlers) typeDefns
    | SynModuleDecl.Exception (exnDefn = SynExceptionDefn (members = members)) ->
        List.iter (visitMemberDefn handlers) members
    | _ -> ()

and visitModuleDecls (handlers : Handlers) (decls : SynModuleDecl list) =
    List.iter (visitModuleDecl handlers) decls

and visitModuleOrNamespace (handlers : Handlers) (SynModuleOrNamespace (decls = decls) : SynModuleOrNamespace) =
    visitModuleDecls handlers decls

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
    let providedArgs = ResizeArray<string * FSharp.Compiler.Text.range * int> ()
    let pipedArgs = ResizeArray<FSharp.Compiler.Text.range * int> ()

    let appHandler : AppHandler =
        fun (ident, r, args) -> providedArgs.Add (ident, r, args)

    let pipeHandler : PipeHandler = fun (r, c) -> pipedArgs.Add (r, c)

    let handlers =
        {
            AppHandler = appHandler
            PipeHandler = pipeHandler
        }

    let symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile ()

    match parseTree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (contents = contents)) ->
        contents |> List.iter (visitModuleOrNamespace handlers)
    | _ -> ()

    let msgs =
        seq {
            for _, range, providedArgsCount in providedArgs do
                let parameterCount = tryGetParameterCount symbolUses range

                match parameterCount with
                | Some paramsCount ->
                    let piped =
                        pipedArgs
                        |> Seq.tryFind (fun (r, _) -> r = range)
                        |> Option.map snd
                        |> Option.defaultValue 0

                    if providedArgsCount + piped < paramsCount then // use LESS, not NOT EQUAL because of CEs, printf, etc. take more than paramsCount
                        let msg =
                            {
                                Type = "Partial Application Analyzer"
                                Message = "Partial application should not be used."
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

[<Literal>]
let name = "PartialAppAnalyzer"

[<Literal>]
let shortDescription = "Warns when partial application is being used."

[<Literal>]
let helpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/PartialAppAnalyzer.html"

[<CliAnalyzer(name, shortDescription, helpUri)>]
let partialAppCliAnalyzer : Analyzer<CliContext> =
    fun context -> async { return analyze context.ParseFileResults.ParseTree context.CheckFileResults }

[<EditorAnalyzer(name, shortDescription, helpUri)>]
let partialAppEditorAnalyzer : Analyzer<EditorContext> =
    fun context ->
        async {
            match context.CheckFileResults with
            | Some checkFileResults -> return analyze context.ParseFileResults.ParseTree checkFileResults
            | None -> return []
        }
