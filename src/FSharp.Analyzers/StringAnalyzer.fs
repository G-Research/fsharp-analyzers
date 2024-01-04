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

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/StringAnalyzer.html"

let tryGetFullName (e : FSharpExpr) =
    if e.Type.ErasedType.HasTypeDefinition then
        e.Type.ErasedType.TypeDefinition.TryGetFullName ()
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

let endsWithAnalyze (typedTree : FSharpImplementationFileContents) =
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

[<Literal>]
let StringEndsWithName = "String.EndsWith Analyzer"

[<Literal>]
let StringEndsWithShortDescription =
    "Verifies the correct usage of System.String.EndsWith"

[<CliAnalyzer(StringEndsWithName, StringEndsWithShortDescription, HelpUri)>]
let endsWithCliAnalyzer (ctx : CliContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map endsWithAnalyze |> Option.defaultValue [] }

[<EditorAnalyzer(StringEndsWithName, StringEndsWithShortDescription, HelpUri)>]
let endsWithEditorAnalyzer (ctx : EditorContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map endsWithAnalyze |> Option.defaultValue [] }

let startsWithAnalyze (typedTree : FSharpImplementationFileContents) =
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

[<Literal>]
let StringStartsWithName = "String.StartsWith Analyzer"

[<Literal>]
let StringStartsWithShortDescription =
    "Verifies the correct usage of System.String.StartsWith"

[<CliAnalyzer(StringStartsWithName, StringStartsWithShortDescription, HelpUri)>]
let startsWithCliAnalyzer (ctx : CliContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map startsWithAnalyze |> Option.defaultValue [] }

[<EditorAnalyzer(StringStartsWithName, StringStartsWithShortDescription, HelpUri)>]
let startsWithEditorAnalyzer (ctx : EditorContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map startsWithAnalyze |> Option.defaultValue [] }

let indexOfAnalyze (typedTree : FSharpImplementationFileContents) =
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

[<Literal>]
let StringIndexOfName = "String.IndexOf Analyzer"

[<Literal>]
let StringIndexOfShortDescription =
    "Verifies the correct usage of System.String.IndexOf"

[<CliAnalyzer(StringIndexOfName, StringIndexOfShortDescription, HelpUri)>]
let indexOfCliAnalyzer (ctx : CliContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map indexOfAnalyze |> Option.defaultValue [] }

[<EditorAnalyzer(StringIndexOfName, StringIndexOfShortDescription, HelpUri)>]
let indexOfEditorAnalyzer (ctx : EditorContext) : Async<Message list> =
    async { return ctx.TypedTree |> Option.map indexOfAnalyze |> Option.defaultValue [] }
