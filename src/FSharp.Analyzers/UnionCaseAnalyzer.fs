namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

module UnionCaseAnalyzer =

    [<Literal>]
    let Code = "GRA-UNIONCASE-001"

    let findAllShadowingCases (ast : ParsedInput) : range list =
        let collector = ResizeArray<range> ()

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

        let handleCase (SynUnionCase (ident = (SynIdent (ident, _)))) =
            if (namesToWarnAbount |> Set.contains ident.idText) then
                collector.Add ident.idRange

            ()

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
                            synUnionCases |> List.iter handleCase
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

                let ranges = findAllShadowingCases ctx.ParseFileResults.ParseTree

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
