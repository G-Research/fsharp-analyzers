module GR.FSharp.Analyzers.LoggingArgFuncNotFullyAppliedAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-LOGARGFUNCFULLAPP-001"

let analyze (typedTree : FSharpImplementationFileContents) =
    let state = ResizeArray<range> ()

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

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (m : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (range : range) =
                let name = String.Join (".", m.DeclaringEntity.Value.FullName, m.DisplayName)
                let assemblyName = "Microsoft.Extensions.Logging.Abstractions"

                let argsContainPartiallyAppliedFunc =
                    args
                    |> List.exists (
                        function
                        | NewArray (_type, exprs) ->
                            exprs
                            |> List.exists (
                                function
                                | Coerce (_type, expr) -> expr.Type.IsFunctionType
                                | _ -> false
                            )
                        | _ -> false
                    )

                if
                    m.Assembly.SimpleName = assemblyName
                    && Set.contains name namesToWarnAbout
                    && argsContainPartiallyAppliedFunc
                then
                    state.Add range
        }

    walkTast walker typedTree

    [
        for range in state do
            {
                Type = "LoggingArgFuncNotFullyAppliedAnalyzer"
                Message =
                    "You have passed a function as one of the display arguments to ILogger.Log{Warning, Error, ...}. You have probably not applied this function fully."
                Code = Code
                Severity = Warning
                Range = range
                Fixes = []
            }
    ]


[<Literal>]
let Name = "LoggingArgFuncNotFullyAppliedAnalyzer"

[<Literal>]
let ShortDescription =
    "Checks if function arguments to ILogging methods are fully applied"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/LoggingArgFuncNotFullyAppliedAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let loggingArgFuncNotFullyAppliedCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let loggingArgFuncNotFullyAppliedEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
