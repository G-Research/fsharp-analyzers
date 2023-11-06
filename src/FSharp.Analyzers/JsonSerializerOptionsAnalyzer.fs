module GR.FSharp.Analyzers.JsonSerializerOptionsAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns


[<Literal>]
let Code = "GRA-JSONOPTS-001"

[<CliAnalyzer("JsonSerializerOptionsAnalyzer", "Scans code for inline usage of JsonSerializerOptions")>]
let jsonSerializerOptionsAnalyzer : Analyzer<CliContext> =
    fun ctx ->
        async {
            let state = ResizeArray<range> ()

            let namesToWarnAbount =
                set
                    [
                        "System.Text.Json.JsonSerializer.Deserialize"
                        "System.Text.Json.JsonSerializer.DeserializeAsync"
                        "System.Text.Json.JsonSerializer.DeserializeAsyncEnumerable"
                        "System.Text.Json.JsonSerializer.Serialize"
                        "System.Text.Json.JsonSerializer.SerializeAsync"
                        "System.Text.Json.JsonSerializer.SerializeToDocument"
                        "System.Text.Json.JsonSerializer.SerializeToElement"
                        "System.Text.Json.JsonSerializer.SerializeToNode"
                        "System.Text.Json.JsonSerializer.SerializeToUtf8Bytes"
                    ]

            let walker =
                { new TypedTreeCollectorBase() with
                    override _.WalkCall (range : range) (m : FSharpMemberOrFunctionOrValue) (args : FSharpExpr list) =
                        let name = String.Join (".", m.DeclaringEntity.Value.FullName, m.DisplayName)
                        let assemblyName = "System.Text.Json"

                        let containsSerOptsCtorCall =
                            args
                            |> List.exists (
                                function
                                | NewObject (objType, _, _) when
                                    objType.FullName = "System.Text.Json.JsonSerializerOptions"
                                    && objType.Assembly.SimpleName = assemblyName
                                    ->
                                    true
                                | _ -> false
                            )

                        if
                            m.Assembly.SimpleName = assemblyName
                            && Set.contains name namesToWarnAbount
                            && containsSerOptsCtorCall
                        then
                            state.Add range
                }

            match ctx.TypedTree with
            | None -> ()
            | Some typedTree -> typedTree.Declarations |> List.iter (walkTast walker)

            return
                state
                |> Seq.map (fun r ->
                    {
                        Type = "JsonSerializerOptions analyzer"
                        Message = "JsonSerializerOptions instances should be cached."
                        Code = Code
                        Severity = Warning
                        Range = r
                        Fixes = []
                    }
                )
                |> Seq.toList
        }
