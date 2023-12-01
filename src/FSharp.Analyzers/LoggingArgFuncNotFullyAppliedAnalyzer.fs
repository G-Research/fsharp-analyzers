module GR.FSharp.Analyzers.LoggingArgFuncNotFullyAppliedAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-LOGARGFUNCFULLAPP-001"

[<CliAnalyzer("LoggingArgFuncNotFullyAppliedAnalyzer",
              "Checks if function arguments to ILogging methods are fully applied",
              "https://g-research.github.io/fsharp-analyzers/analyzers/LoggingArgFuncNotFullyAppliedAnalyzer.html")>]
let loggingArgFuncNotFullyAppliedAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {
            let state = ResizeArray<range> ()

            let namesToWarnAbount =
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

            let partialApps =
                PartialAppAnalyzer.analyze ctx.ParseFileResults.ParseTree ctx.CheckFileResults

            let walker =
                { new TypedTreeCollectorBase() with
                    override _.WalkCall
                        _
                        (m : FSharpMemberOrFunctionOrValue)
                        _
                        _
                        (args : FSharpExpr list)
                        (range : range)
                        =
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
                                        | Coerce (_type, expr) ->
                                            partialApps
                                            |> Seq.exists (fun m -> Range.rangeContainsRange expr.Range m.Range)
                                        | _ -> false
                                    )
                                | _ -> false
                            )

                        if
                            m.Assembly.SimpleName = assemblyName
                            && Set.contains name namesToWarnAbount
                            && argsContainPartiallyAppliedFunc
                        then
                            state.Add range
                }

            ctx.TypedTree |> Option.iter (walkTast walker)

            return
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
        }
