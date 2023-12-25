module GR.FSharp.Analyzers.TopLevelTypedAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Syntax
open TopLevelTypeAnalysis

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

            let missingInfo =
                findMissingTypeInformation
                    ctx.SourceText
                    ctx.ParseFileResults.ParseTree
                    ctx.CheckFileResults
                    ctx.CheckProjectResults

            return
                missingInfo
                |> List.map (fun missingInfo ->
                    let msg =
                        let parameters () =
                            missingInfo.Parameters
                            |> List.map (fun p ->
                                match p with
                                | MissingParameterType.SingleParameter { ParameterName = name } ->
                                    $"Add type annotation for parameter `%s{name}`."
                                | MissingParameterType.SimpleTupleParameter (items = items) ->
                                    items
                                    |> List.map (fun { ParameterName = name } ->
                                        $"Add type annotation for parameter `%s{name}`."
                                    )
                                    |> String.concat "\n"
                                | MissingParameterType.Pattern { Index = idx } ->
                                    $"Add type annotation for parameter at index `%i{idx}`."
                            )
                            |> String.concat "\n"

                        let genericParameters () =
                            missingInfo.GenericParameters
                            |> List.map (fun gp ->
                                if gp.MissingConstraints.IsEmpty then
                                    $"Generic type parameter %s{gp.Name} not present."
                                else
                                    gp.MissingConstraints
                                    |> List.map (sprintf "Missing constraint: %s")
                                    |> String.concat ". "
                            )
                            |> String.concat "\n"

                        let identifier =
                            match missingInfo.Declaration with
                            | Declaration.Binding (name = ident) -> ident.idText
                            | Declaration.ImplicitCtor (typeName = ident) -> $"The constructor of %s{ident.idText}"
                            | Declaration.AutoProperty (name, _) -> name.idText

                        [
                            $"`%s{identifier}` is not private"
                            ", not fully typed"
                            if missingInfo.ValueCouldBeMadePrivateToFile then
                                $" and could be made private to file."
                            if not missingInfo.Parameters.IsEmpty then
                                "\n"
                                parameters ()
                            if not missingInfo.GenericParameters.IsEmpty then
                                "\n"
                                genericParameters ()
                            match missingInfo.Declaration with
                            | Declaration.Binding (returnType = Some _) -> "\nSpecify the return type."
                            | _ -> ()
                        ]
                        |> String.concat ""

                    let m =
                        match missingInfo.Declaration with
                        | Declaration.Binding (name = ident)
                        | Declaration.ImplicitCtor (typeName = ident) -> ident.idRange
                        | Declaration.AutoProperty (name, _) -> name.idRange

                    {
                        Type = "TopLevelTyped analyzer"
                        Message = msg
                        Code = Code
                        Severity = Warning
                        Range = m
                        Fixes = []
                    }
                )
        }
