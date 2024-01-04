module GR.FSharp.Analyzers.JsonSerializerOptionsAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns


[<Literal>]
let Code = "GRA-JSONOPTS-001"

let analyze (typedTree : FSharpImplementationFileContents) =
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
            override _.WalkCall _ (m : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (range : range) =
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

    walkTast walker typedTree

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

[<Literal>]
let Name = "JsonSerializerOptionsAnalyzer"

[<Literal>]
let ShortDescription = "Scans code for inline usage of JsonSerializerOptions"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/JsonSerializerOptionsAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let jsonSerializerOptionsCliAnalyzer : Analyzer<CliContext> =
    fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let jsonSerializerOptionsEditorAnalyzer : Analyzer<EditorContext> =
    fun ctx -> async { return ctx.TypedTree |> Option.map analyze |> Option.defaultValue [] }
