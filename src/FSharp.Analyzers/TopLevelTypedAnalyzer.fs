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
                        |> List.indexed
                        |> List.choose (fun (idx, pat) ->
                            let pat = removePatternParens pat

                            match pat with
                            | SynPat.Typed (targetType = targetType) ->
                                // TODO: the type might be incomplete, if it has wildcard for example.
                                None
                            | untypedPat -> Some (idx, untypedPat)
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
                        let andOrComma, notUsedOutsideFile =
                            if not hasUsesOutsideOfFile then
                                ", ", $" and not used outside `%s{ctx.FileName}`"
                            else
                                "and", ""

                        let addReturnType = if hasReturnType then "" else "Specify the return type."

                        let addTypeForParameters =
                            let text =
                                untypedParameters
                                |> List.map (fun (idx, pat) ->
                                    match pat with
                                    | SynPat.Named (ident = SynIdent (ident = ident)) -> ident.idText
                                    | _ -> $"parameter at index %i{idx}"
                                    |> sprintf "Add type annotation for parameter `%s`"
                                )
                                |> String.concat "\n"

                            if String.IsNullOrWhiteSpace text then
                                ""
                            else
                                String.Concat (" ", text, ".")


                        $"`%s{ident.idText}` is not private%s{andOrComma}not fully typed%s{notUsedOutsideFile}.%s{addReturnType}%s{addTypeForParameters}"

                    {
                        Type = "TopLevelTyped analyzer"
                        Message = msg
                        Code = Code
                        Severity = Warning
                        Range = ident.idRange
                        Fixes = []
                    }
                    |> messages.Add

            let processMember (md : SynMemberDefn) =
                match md with
                | SynMemberDefn.Open _ -> ()
                | SynMemberDefn.Member (memberDefn = memberDefn) -> processBinding memberDefn
                | SynMemberDefn.GetSetMember (memberDefnForGet, memberDefnForSet, range, trivia) -> failwith "todo"
                | SynMemberDefn.ImplicitCtor (accessibility, attributes, ctorArgs, selfIdentifier, xmlDoc, range, trivia) ->
                    failwith "todo"
                | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, inheritAlias, range) -> failwith "todo"
                | SynMemberDefn.LetBindings (bindings, isStatic, isRecursive, range) -> failwith "todo"
                | SynMemberDefn.AbstractSlot (slotSig, flags, range, trivia) -> failwith "todo"
                | SynMemberDefn.Interface (interfaceType, withKeyword, members, range) -> failwith "todo"
                | SynMemberDefn.Inherit (baseType, asIdent, range) -> failwith "todo"
                | SynMemberDefn.ValField (fieldInfo, range) -> failwith "todo"
                | SynMemberDefn.NestedType (typeDefn, accessibility, range) -> failwith "todo"
                | SynMemberDefn.AutoProperty (attributes,
                                              isStatic,
                                              ident,
                                              typeOpt,
                                              propKind,
                                              memberFlags,
                                              memberFlagsForSet,
                                              xmlDoc,
                                              accessibility,
                                              synExpr,
                                              range,
                                              trivia) -> failwith "todo"

            let walker =
                { new SyntaxCollectorBase() with
                    override x.WalkSynModuleDecl (path, mdl) =
                        match mdl with
                        | SynModuleDecl.Let (bindings = bindings) -> List.iter processBinding bindings
                        | SynModuleDecl.Types (typeDefns = typeDefns) ->
                            for (SynTypeDefn (typeRepr = typeRepr ; members = additionalMembers)) in typeDefns do
                                match typeRepr with
                                | SynTypeDefnRepr.Simple _
                                | SynTypeDefnRepr.Exception _ -> ()
                                | SynTypeDefnRepr.ObjectModel (members = members) -> List.iter processMember members

                                List.iter processMember additionalMembers
                        | _ -> ()
                }

            walkAst walker ctx.ParseFileResults.ParseTree

            return Seq.toList messages
        }
