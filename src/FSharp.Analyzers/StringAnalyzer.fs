module GR.FSharp.Analyzers.StringAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

[<Literal>]
let StringEndsWithCode = "GRA-STRING-001"

[<Literal>]
let StringStartsWithCode = "GRA-STRING-002"

[<Literal>]
let StringIndexOfCode = "GRA-STRING-003"

let tryGetFullName (e : FSharpExpr) =
    if e.Type.ErasedType.HasTypeDefinition then
        match e.Type.ErasedType.TypeDefinition.TryGetFullName () with
        | Some n -> Some n
        | _ -> None
    else
        None

let (|StringExpr|_|) (e : FSharpExpr) =
    match tryGetFullName e with
    | Some "System.String" -> Some ()
    | _ -> None

let (|IntExpr|_|) (e : FSharpExpr) =
    if e.Type.ErasedType.HasTypeDefinition then
        match tryGetFullName e with
        | Some "System.Int32" -> Some ()
        | _ -> None
    else
        None

let invalidStringFunctionUseAnalyzer
    functionName
    code
    message
    severity
    (typedTree : FSharpImplementationFileContents)
    (typedArgumentPredicate : FSharpExpr list -> bool)
    =
    let invocations = ResizeArray<range> ()

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (m : range) =
                if
                    (mfv.Assembly.SimpleName = "System.Runtime"
                     || mfv.Assembly.SimpleName = "netstandard")
                    && mfv.FullName = $"System.String.{functionName}"
                    && typedArgumentPredicate args
                then
                    invocations.Add m
        }

    walkTast walker typedTree

    invocations
    |> Seq.map (fun mFunctionName ->
        {
            Type = $"String.{functionName} analyzer"
            Message = message
            Code = code
            Severity = severity
            Range = mFunctionName
            Fixes = []
        }
    )
    |> Seq.toList

[<CliAnalyzer("String.EndsWith Analyzer",
              "Verifies the correct usage of System.String.EndsWith",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
let endsWithAnalyzer (ctx : CliContext) : Async<Message list> =
    async {
        match ctx.TypedTree with
        | None -> return List.empty
        | Some typedTree ->

        return
            invalidStringFunctionUseAnalyzer
                "EndsWith"
                StringEndsWithCode
                "The usage of String.EndsWith with a single string argument is discouraged. Signal your intention explicitly by calling an overload."
                Warning
                typedTree
                (fun (args : FSharpExpr list) ->
                    match args with
                    | [ StringExpr ] -> true
                    | _ -> false
                )
    }

[<CliAnalyzer("String.StartsWith Analyzer",
              "Verifies the correct usage of System.String.StartsWith",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
let startsWithAnalyzer (ctx : CliContext) : Async<Message list> =
    async {
        match ctx.TypedTree with
        | None -> return List.empty
        | Some typedTree ->
            return
                invalidStringFunctionUseAnalyzer
                    "StartsWith"
                    StringStartsWithCode
                    "The usage of String.StartsWith with a single string argument is discouraged. Signal your intention explicitly by calling an overload."
                    Warning
                    typedTree
                    (fun (args : FSharpExpr list) ->
                        match args with
                        | [ StringExpr ] -> true
                        | _ -> false
                    )
    }

[<CliAnalyzer("String.IndexOf Analyzer",
              "Verifies the correct usage of System.String.IndexOf",
              "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html")>]
let indexOfAnalyzer (ctx : CliContext) : Async<Message list> =
    async {
        match ctx.TypedTree with
        | None -> return List.empty
        | Some typedTree ->

        return
            invalidStringFunctionUseAnalyzer
                "IndexOf"
                StringIndexOfCode
                "The usage of String.IndexOf with a single string argument is discouraged. Signal your intention explicitly by calling an overload."
                Warning
                typedTree
                (fun args ->
                    match args with
                    | [ StringExpr ]
                    | [ StringExpr ; IntExpr ]
                    | [ StringExpr ; IntExpr ; IntExpr ] -> true
                    | _ -> false
                )
    }
