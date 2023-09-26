namespace ``G-Research``.FSharp.Analyzers

open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

module UnionCaseAnalyzer =

    [<Literal>]
    let Code = "GRA-004"

    let findAllInvocations (ast : ParsedInput) : range list =
        let collector = ResizeArray<range> ()
        let namesToWarnAbount = set [ "None" ; "Some" ]

        let checkCases (SynUnionCase (ident = (SynIdent (ident, _)))) =
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
                            synUnionCases |> List.iter checkCases
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

                let ranges = findAllInvocations ctx.ParseFileResults.ParseTree

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
