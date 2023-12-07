module GR.FSharp.Analyzers.TypedInterpolatedStringsAnalyzer

open System.Text.RegularExpressions
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.Syntax

[<Literal>]
let Code = "GRA-INTERPOLATED-001"

/// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf
let specifierRegex =
    Regex (@"\%(\+|\-)?\d*\.?\d*(b|s|c|d|i|u|x|X|o|B|e|E|f|F|g|G|M|O|A)$")

[<CliAnalyzer("TypedInterpolatedStringsAnalyzer",
              "TODO: add description.",
              "https://g-research.github.io/fsharp-analyzers/analyzers/TypedInterpolatedStringsAnalyzer.html")>]
let typedInterpolatedStringsAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {
            let messages = ResizeArray<Message> ()

            let walker =
                { new SyntaxCollectorBase() with
                    override x.WalkExpr (expr : SynExpr) =
                        match expr with
                        | SynExpr.InterpolatedString (contents = contents) ->
                            contents
                            |> List.pairwise
                            |> List.iter (fun (p1, p2) ->
                                match p1, p2 with
                                | SynInterpolatedStringPart.String (value = s),
                                  SynInterpolatedStringPart.FillExpr (fillExpr = e) ->
                                    if not (isNull s) && not (specifierRegex.IsMatch s) then
                                        messages.Add
                                            {
                                                Type = "TypedInterpolatedStringsAnalyzer"
                                                Message =
                                                    "Interpolated hole expression without format detected. Use prefix with the correct % to enforce type safety."
                                                Code = Code
                                                Severity = Warning
                                                Range = e.Range
                                                Fixes = []
                                            }
                                | _ -> ()
                            )
                        | _ -> ()
                }

            walkAst walker ctx.ParseFileResults.ParseTree

            return Seq.toList messages
        }
