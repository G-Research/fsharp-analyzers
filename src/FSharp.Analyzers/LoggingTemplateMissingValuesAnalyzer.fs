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

let rec (|StringFormat|_|) (e : FSharpExpr) =
    match e with
    | Call (_exprOption, _mfv, _types, _l, exprs) ->
        match exprs with
        | Coerce (targetType, expr) :: _ when
            targetType.BasicQualifiedName = "Microsoft.FSharp.Core.PrintfModule+StringFormat`1"
            ->
            match expr with
            | NewObject (_mfv, _types, [ StringConst s ]) -> Some s
            | _ -> None
        | exprs ->
            exprs
            |> List.tryPick (
                function
                | StringConst s -> Some s
                | _ -> None
            )
        | _ -> None
    | _ -> None

and (|StringConst|_|) (e : FSharpExpr) =
    let name = e.Type.ErasedType.TypeDefinition.TryGetFullName ()

    match name, e with
    | Some "System.String", Const (o, _type) when not (isNull o) -> Some (string o)
    | Some "System.String", Application (expr, _, _) ->
        match expr with
        | Let ((_mfv, StringFormat s, _debugPointAtBinding), _expr) -> Some s
        | _ -> None
    | _, StringFormat s -> Some s
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

    let pattern = @"(?<opening>{+)[^{}]*(?<closing>}+)"
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

                let calcExpected () =
                    let logString =
                        args
                        |> List.tryPick (
                            function
                            | StringConst s -> Some s
                            | _ -> None
                        )

                    match logString with
                    | Some s ->
                        let matches =
                            regex.Matches s
                            |> Seq.filter (fun matchItem ->
                                matchItem.Groups["opening"].Value.Length = matchItem.Groups["closing"].Value.Length
                            )
                            |> Seq.toArray

                        let escapedMatches =
                            Array.sumBy
                                (fun (matchItem : Match) ->
                                    let opening = matchItem.Groups["opening"]
                                    let closing = matchItem.Groups["closing"]
                                    let isEscaped = opening.Value.Length % 2 = 0 && closing.Value.Length % 2 = 0
                                    if isEscaped then 1 else 0
                                )
                                matches

                        matches.Length - escapedMatches
                    | None -> 0

                if
                    m.Assembly.SimpleName = assemblyName
                    && Set.contains name namesToWarnAbout
                    && provided <> calcExpected ()
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
