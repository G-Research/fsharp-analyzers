module GR.FSharp.Analyzers.LoggingTemplateMissingValuesAnalyzer

open System
open System.Text.RegularExpressions
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-LOGTEMPLMISSVALS-001"

let (|StringConst|_|) (e : FSharpExpr) =
    let name = e.Type.ErasedType.TypeDefinition.TryGetFullName ()

    match name, e with
    | Some "System.String", Const (o, _type) when not (isNull o) -> Some (string o)
    | _ -> None


let analyze (typedTree : FSharpImplementationFileContents) =
    let state = ResizeArray<range * string> ()

    let namesToWarnAbout =
        set
            [
                "Microsoft.Extensions.Logging.LoggerExtensions.Log"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogCritical"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogDebug"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogError"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogInformation"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogTrace"
                "Microsoft.Extensions.Logging.LoggerExtensions.LogWarning"
            ]

    let pattern = @"(?<opening>{+)[a-zA-Z0-9_-]*(?<closing>}+)"
    let regex = Regex pattern

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (m : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (range : range) =
                let name = String.Join (".", m.DeclaringEntity.Value.FullName, m.DisplayName)
                let assemblyName = "Microsoft.Extensions.Logging.Abstractions"

                let provided =
                    if args.Length >= 2 then
                        match List.tryLast args with
                        | Some (NewArray (_type, exprs)) -> List.length exprs
                        | _ -> 0
                    else
                        0

                let expected =
                    let logString =
                        args
                        |> List.tryPick (
                            function
                            | StringConst s -> Some s
                            | _ -> None
                        )

                    match logString with
                    | Some s ->
                        let matches = regex.Matches s

                        let escapedMatches =
                            Seq.sumBy
                                (fun (matchItem : Match) ->
                                    let opening = matchItem.Groups["opening"]
                                    let closing = matchItem.Groups["closing"]
                                    let isEscaped = opening.Value.Length % 2 = 0 && closing.Value.Length % 2 = 0
                                    if isEscaped then 1 else 0
                                )
                                matches

                        matches.Count - escapedMatches
                    | None -> 0

                if
                    m.Assembly.SimpleName = assemblyName
                    && Set.contains name namesToWarnAbout
                    && provided <> expected
                then
                    state.Add (range, m.DisplayName)
        }

    walkTast walker typedTree

    [
        for range, name in state do
            {
                Type = "LoggingTemplateMissingValuesAnalyzer"
                Message = $"The given values in your call to ILogger.%s{name} don't match the expected templated args."
                Code = Code
                Severity = Severity.Warning
                Range = range
                Fixes = []
            }
    ]


[<Literal>]
let Name = "LoggingTemplateMissingValuesAnalyzer"

[<Literal>]
let ShortDescription =
    "Checks if all templated args in a log message have been given values or too many values have been given"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/LoggingTemplateMissingValuesAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let loggingTemplateMissingValuesCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let loggingTemplateMissingValuesEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
