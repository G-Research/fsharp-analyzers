module GR.FSharp.Analyzers.TypedInterpolatedStringsAnalyzer

open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-INTERPOLATED-001"

/// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/plaintext-formatting#format-specifiers-for-printf
let specifierRegex =
    Regex (@"\%(\+|\-)?\d*\.?\d*(b|s|c|d|i|u|x|X|o|B|e|E|f|F|g|G|M|O|A)$")

let analyze (parseTree : ParsedInput) (typedTree : FSharpImplementationFileContents) =
    let messages = ResizeArray<Message> ()
    // Formattable strings needs to be skipped https://learn.microsoft.com/en-us/dotnet/api/system.formattablestring?view=net-8.0
    let formattableStrings = HashSet Range.comparer

    let tastWalker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall objExprOpt memberOrFunc objExprTypeArgs memberOrFuncTypeArgs argExprs exprRange =
                if
                    memberOrFunc.FullName = "System.Runtime.CompilerServices.FormattableStringFactory.Create"
                    && argExprs.Length = 2
                    && argExprs.[0].Type.ErasedType.BasicQualifiedName = "System.String"
                then
                    formattableStrings.Add argExprs.[0].Range |> ignore
        }

    walkTast tastWalker typedTree

    let walker =
        { new SyntaxCollectorBase() with
            override x.WalkExpr (_, expr : SynExpr) =
                match expr with
                | SynExpr.InterpolatedString (contents = contents) when not (formattableStrings.Contains expr.Range) ->
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
                                        Severity = Severity.Warning
                                        Range = e.Range
                                        Fixes = []
                                    }
                        | _ -> ()
                    )
                | _ -> ()
        }

    walkAst walker parseTree

    Seq.toList messages


[<Literal>]
let Name = "TypedInterpolatedStringsAnalyzer"

[<Literal>]
let ShortDescription = "Warns about missing type specifiers in interpolated strings"

[<Literal>]
let helpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/TypedInterpolatedStringsAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, helpUri)>]
let typedInterpolatedStringsCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {
            return
                ctx.TypedTree
                |> Option.map (analyze ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }

[<EditorAnalyzer(Name, ShortDescription, helpUri)>]
let typedInterpolatedStringsEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) ->
        async {
            return
                ctx.TypedTree
                |> Option.map (analyze ctx.ParseFileResults.ParseTree)
                |> Option.defaultValue []
        }
