module GR.FSharp.Analyzers.TopLevelTypedAnalyzer

open System
open System.IO
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

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
                TopLevelTypeAnalysis.findMissingTypeInformation
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
                                p.ParameterName
                                |> Option.defaultValue $"parameter at index %i{p.Index}"
                                |> sprintf "Add type annotation for parameter `%s`"
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

                        [
                            $"`%s{missingInfo.NameIdent.idText}` is not private"
                            ", not fully typed"
                            if missingInfo.ValueIsUsedOutsideTheProject then
                                $" and used outside `%s{ctx.FileName}`."
                            else
                                $" and not used outside `%s{ctx.FileName}` in the current project."
                            if not missingInfo.Parameters.IsEmpty then
                                "\n"
                                parameters ()
                            if not missingInfo.GenericParameters.IsEmpty then
                                "\n"
                                genericParameters ()
                            match missingInfo.ReturnType with
                            | None -> ()
                            | Some _ -> "\nSpecify the return type."
                        ]
                        |> String.concat ""

                    {
                        Type = "TopLevelTyped analyzer"
                        Message = msg
                        Code = Code
                        Severity = Warning
                        Range = missingInfo.NameIdent.idRange
                        Fixes = []
                    }
                )
        }
