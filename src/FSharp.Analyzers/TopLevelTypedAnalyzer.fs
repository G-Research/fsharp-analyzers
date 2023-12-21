module GR.FSharp.Analyzers.TopLevelTypedAnalyzer

open System
open System.IO
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

let rec private removePatternParens (p : SynPat) =
    match p with
    | SynPat.Paren (pat = inner) -> removePatternParens inner
    | _ -> p

[<Literal>]
let Code : string = "GRA-UNTYPEDAPI-001"

[<CliAnalyzer("TopLevelTypedAnalyzer",
              "Verifies what top-level functions are not typed.",
              "https://g-research.github.io/fsharp-analyzers/analyzers/TopLevelTypedAnalyzer.html")>]
let topLevelTypedAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {
            let hasSignatureFile () =
                let signatureFileName = String.Concat (ctx.FileName, "i")
                Array.contains signatureFileName ctx.CheckProjectResults.ProjectContext.ProjectOptions.SourceFiles

            match ctx.ParseFileResults.ParseTree with
            | ParsedInput.SigFile _ -> return List.empty<Message>
            | ParsedInput.ImplFile _ when hasSignatureFile () -> return List.empty<Message>
            | ParsedInput.ImplFile _ ->

            let messages = ResizeArray<Message> ()

            let processBinding (SynBinding (headPat = headPat ; returnInfo = returnInfo)) =
                let isPrivate =
                    match headPat with
                    | SynPat.Named (accessibility = Some (SynAccess.Private _))
                    | SynPat.LongIdent (accessibility = Some (SynAccess.Private _)) -> true
                    | _ -> false

                let ident =
                    match headPat with
                    | SynPat.Named (ident = SynIdent (ident = ident)) -> ident
                    | SynPat.LongIdent (longDotId = sli) ->
                        sli.LongIdent
                        |> List.tryLast
                        |> Option.defaultWith (fun () -> failwithf $"Could not find a name for %A{headPat.Range}")
                    | _ -> failwithf $"Could not find a name for %A{headPat.Range}"

                let symbol =
                    let line = ctx.SourceText.GetLineString (ident.idRange.EndLine - 1)

                    ctx.CheckFileResults.GetSymbolUseAtLocation (
                        ident.idRange.EndLine,
                        ident.idRange.EndColumn,
                        line,
                        [ ident.idText ]
                    )
                    |> Option.defaultWith (fun () -> failwithf $"Could not find symbol for %s{ident.idText}")

                let hasUsesOutsideOfFile =
                    ctx.CheckProjectResults.GetUsesOfSymbol (symbol.Symbol)
                    |> Array.exists (fun symbolUse -> symbolUse.FileName <> ctx.FileName)

                // Does every parameter has a fully type?
                let untypedParameters =
                    match headPat with
                    | SynPat.LongIdent (argPats = SynArgPats.Pats (pats = pats)) ->
                        pats
                        |> List.choose (fun pat ->
                            let pat = removePatternParens pat

                            match pat with
                            | SynPat.Typed (targetType = targetType) ->
                                // TODO: the type might be incomplete, if it has wildcard for example.
                                None
                            | untypedPat -> Some untypedPat
                        )
                    | _ ->
                        // There were no parameters
                        []

                // Is there a return type?
                let hasReturnType =
                    // TODO: again incomplete, type cannot have wildcards.
                    Option.isSome returnInfo

                // Are all potential inferred constraints present?
                let isNotPrivateAndLacksTypeInformation =
                    not isPrivate && (not (List.isEmpty untypedParameters) || not hasReturnType)

                if isNotPrivateAndLacksTypeInformation then
                    let msg =
                        if not hasUsesOutsideOfFile then
                            let projectName =
                                Path.GetFileName ctx.CheckProjectResults.ProjectContext.ProjectOptions.ProjectFileName

                            $"`%s{ident.idText}` is not private and not fully typed and not used outside %s{ctx.FileName} in project %s{projectName}. Add `private` keyword."
                        else
                            $"`%s{ident.idText}` is not private and not fully typed. Add fully type information."

                    {
                        Type = "TopLevelTyped analyzer"
                        Message = msg
                        Code = Code
                        Severity = Warning
                        Range = ident.idRange
                        Fixes = []
                    }
                    |> messages.Add

            let walker =
                { new SyntaxCollectorBase() with
                    override x.WalkSynModuleDecl (path, mdl) =
                        match mdl with
                        | SynModuleDecl.Let (bindings = bindings) -> List.iter processBinding bindings
                        | SynModuleDecl.Types (typeDefns = typeDefns) -> ()
                        | _ -> ()
                }

            walkAst walker ctx.ParseFileResults.ParseTree

            return Seq.toList messages
        }
