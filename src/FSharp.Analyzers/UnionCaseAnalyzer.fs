namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

module UnionCaseAnalyzer =

    [<Literal>]
    let Code = "GRA-UNIONCASE-001"

    let findAllShadowingCases
        (checkFileResults : FSharpCheckFileResults)
        (parseFileResults : FSharpParseFileResults)
        (ast : ParsedInput)
        : range list
        =
        let collector = ResizeArray<range> ()

        let declarationListItems =
            let partialLongName : PartialLongName =
                {
                    QualifyingIdents = [ "FSharp" ; "Core" ]
                    PartialIdent = ""
                    EndColumn = 0
                    LastDotPos = None
                }

            let info =
                checkFileResults.GetDeclarationListInfo (Some parseFileResults, 1, "", partialLongName)

            info.Items
            |> Array.filter (fun i ->
                i.FullName.StartsWith ("Microsoft.FSharp.Core")
                && i.Glyph = FSharpGlyph.EnumMember
            )

        let checkCase (SynUnionCase (ident = (SynIdent (ident, _)))) =
            let isCaseNameInFsharpCore =
                declarationListItems
                |> Array.exists (fun i -> i.FullName.EndsWith ($".{ident.idText}"))

            if isCaseNameInFsharpCore then
                collector.Add ident.idRange

        let walker =
            { new SyntaxCollectorBase() with
                override _.WalkTypeDefn
                    (SynTypeDefn (typeInfo = SynComponentInfo (attributes = synAttributeLists) ; typeRepr = repr))
                    =
                    let hasReqQualAccAttribute =
                        synAttributeLists
                        |> List.exists (fun lst ->
                            lst.Attributes
                            |> Seq.exists (fun (a : SynAttribute) ->
                                a.TypeName.LongIdent[0].idText = "RequireQualifiedAccess"
                            )
                        )

                    if not hasReqQualAccAttribute then
                        match repr with
                        | SynTypeDefnRepr.Simple (SynTypeDefnSimpleRepr.Union (unionCases = synUnionCases), _) ->
                            synUnionCases |> List.iter checkCase
                        | _ -> ()
                    else
                        ()
            }

        walkAst walker ast

        collector |> Seq.toList

    [<CliAnalyzer "UnionCaseAnalyzer">]
    let unionCaseAnalyzer : Analyzer<CliContext> =
        fun ctx ->
            async {

                let ranges =
                    findAllShadowingCases ctx.CheckFileResults ctx.ParseFileResults ctx.ParseFileResults.ParseTree

                let msgs =
                    ranges
                    |> List.map (fun r ->
                        {
                            Type = "UnionCase analyzer"
                            Message =
                                "This discriminated union contains a case with the same name as a case from FSharp.Core. Consider renaming it or applying RequireQualifiedAccess to avoid clashes."
                            Code = Code
                            Severity = Warning
                            Range = r
                            Fixes = []
                        }
                    )

                return msgs
            }
