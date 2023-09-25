namespace ``G-Research``.FSharp.Analyzers

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns
open TASTCollection

module JsonSerializerOptionsAnalyzer =

    [<Literal>]
    let Code = "GRA-002"

    [<CliAnalyzer "JsonSerializerOptionsAnalyzer">]
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

                let handler : Handler =
                    let callHandler (range : range) (m : FSharpMemberOrFunctionOrValue) (args : FSharpExpr list) =
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

                    Handler.CallHandler callHandler

                match ctx.TypedTree with
                | None -> ()
                | Some typedTree -> typedTree.Declarations |> List.iter (visitDeclaration handler)

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
