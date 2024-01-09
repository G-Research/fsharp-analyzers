module GR.FSharp.Analyzers.UnionCaseAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-UNIONCASE-001"

let findAllShadowingCases
    (ast : ParsedInput)
    (checkFileResults : FSharpCheckFileResults)
    (sourceText : ISourceText)
    : (range * Fix) list
    =
    let collector = ResizeArray<range * Fix> ()

    let namesToWarnAbount =
        set
            [
                "Choice1Of2"
                "Choice2Of2"
                "Choice1Of3"
                "Choice2Of3"
                "Choice3Of3"
                "Choice1Of4"
                "Choice2Of4"
                "Choice3Of4"
                "Choice4Of4"
                "Choice1Of5"
                "Choice2Of5"
                "Choice3Of5"
                "Choice4Of5"
                "Choice5Of5"
                "Choice1Of6"
                "Choice2Of6"
                "Choice3Of6"
                "Choice4Of6"
                "Choice5Of6"
                "Choice6Of6"
                "Choice1Of7"
                "Choice2Of7"
                "Choice3Of7"
                "Choice4Of7"
                "Choice5Of7"
                "Choice6Of7"
                "Choice7Of7"
                "None"
                "Some"
                "ValueNone"
                "ValueSome"
                "Ok"
                "Error"
            ]

    let handleCase (leadingKeyWord : SynTypeDefnLeadingKeyword) (SynUnionCase (ident = (SynIdent (ident, _)))) =
        if (namesToWarnAbount |> Set.contains ident.idText) then
            let fromRange, toText =
                match leadingKeyWord with
                | SynTypeDefnLeadingKeyword.And _ -> leadingKeyWord.Range.EndRange, " [<RequireQualifiedAccess>]"
                | SynTypeDefnLeadingKeyword.Type _ ->
                    let indent = String.replicate leadingKeyWord.Range.StartRange.StartColumn " "
                    leadingKeyWord.Range.StartRange, $"[<RequireQualifiedAccess>]\n%s{indent}"
                | _ -> failwith "SynTypeDefnLeadingKeyword case not supported"

            let fix =
                {
                    FromText = ""
                    FromRange = fromRange
                    ToText = toText
                }

            collector.Add (ident.idRange, fix)

        ()

    let walker =
        { new SyntaxCollectorBase() with
            override _.WalkTypeDefn
                (
                    _,
                    SynTypeDefn (
                        typeInfo = SynComponentInfo (attributes = synAttributeLists) ; typeRepr = repr ; trivia = trivia)
                )
                =
                let hasReqQualAccAttribute =
                    synAttributeLists
                    |> List.exists (fun lst ->
                        lst.Attributes
                        |> List.exists (fun (a : SynAttribute) ->
                            let lineText = sourceText.GetLineString (a.Range.EndLine - 1)
                            let name = (List.last a.TypeName.LongIdent).idText

                            let symbolUseOpt =
                                checkFileResults.GetSymbolUseAtLocation (
                                    a.Range.EndLine,
                                    a.Range.EndColumn,
                                    lineText,
                                    [ name ]
                                )

                            match symbolUseOpt with
                            | Some symbolUse ->
                                match symbolUse.Symbol with
                                | :? FSharpMemberOrFunctionOrValue as mfv ->
                                    mfv.FullName = "Microsoft.FSharp.Core.RequireQualifiedAccessAttribute"
                                | _ -> false
                            | _ -> false
                        )
                    )

                if not hasReqQualAccAttribute then
                    match repr with
                    | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (unionCases = synUnionCases), _) ->
                        synUnionCases |> List.iter (handleCase trivia.LeadingKeyword)
                    | _ -> ()
                else
                    ()
        }

    walkAst walker ast

    collector |> Seq.toList

let analyze parseTree sourceText checkFileResults =
    let rangesAndFixes = findAllShadowingCases parseTree checkFileResults sourceText

    let msgs =
        rangesAndFixes
        |> List.map (fun (r, f) ->
            {
                Type = "UnionCase analyzer"
                Message =
                    "This discriminated union contains a case with the same name as a case from FSharp.Core. Consider renaming it or applying RequireQualifiedAccess to avoid clashes."
                Code = Code
                Severity = Warning
                Range = r
                Fixes = [ f ]
            }
        )

    msgs

[<Literal>]
let Name = "UnionCaseAnalyzer"

[<Literal>]
let ShortDescription =
    "Warns for reusing any default FSharp.Core union case name without RequireQualifiedAccessAttribute"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/UnionCaseAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let unionCaseCliAnalyzer : Analyzer<CliContext> =
    fun ctx -> async { return analyze ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let unionCaseEditorAnalyzer : Analyzer<EditorContext> =
    fun ctx ->
        async {
            return
                ctx.CheckFileResults
                |> Option.map (analyze ctx.ParseFileResults.ParseTree ctx.SourceText)
                |> Option.defaultValue []
        }
