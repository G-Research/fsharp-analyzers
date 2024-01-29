/// This analyzer works on the assumption, that the Seq module functions result in virtual IL calls in contrast to the
/// equivalent functions from the Array,List,Set modules.
module GR.FSharp.Analyzers.VirtualCallAnalyzer

open System
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text
open Util

[<Literal>]
let Code = "GRA-VIRTUALCALL-001"

let (|CoerceToSeq|_|) (includeFromSet : bool) (expr : FSharpExpr) =
    match expr with
    | Coerce (t, e) when
        t.HasTypeDefinition
        && t.TypeDefinition.LogicalName = "seq`1"
        && e.Type.HasTypeDefinition
        ->
        let coercedValueTypeName =
            if
                e.Type.TypeDefinition.IsFSharpAbbreviation
                && e.Type.TypeDefinition.AbbreviatedType.HasTypeDefinition
                && e.Type.TypeDefinition.AbbreviatedType.BasicQualifiedName.StartsWith (
                    "Microsoft.FSharp",
                    StringComparison.Ordinal
                )
            then
                e.Type.TypeDefinition.AbbreviatedType.TypeDefinition.LogicalName
            else
                e.Type.TypeDefinition.LogicalName

        match coercedValueTypeName with
        | "[]`1"
        | "array`1" -> Some "Array"
        | "list`1"
        | "List`1" -> Some "List"
        | "Set`1" when includeFromSet -> Some "Set"
        | _ -> None
    | _ -> None

let seqFuncsWithEquivalentsInAllCollections =
    set
        [
            "Microsoft.FSharp.Collections.Seq.length" // ~ Set.count
            "Microsoft.FSharp.Collections.Seq.max" // ~ Set.maxElement
            "Microsoft.FSharp.Collections.Seq.min" // ~ Set.minElement
            "Microsoft.FSharp.Collections.Seq.contains"
            "Microsoft.FSharp.Collections.Seq.exists"
            "Microsoft.FSharp.Collections.Seq.foldBack"
            "Microsoft.FSharp.Collections.Seq.fold"
        ]

let seqFuncsWithEquivalentsInArrayAndList =
    set
        [
            "Microsoft.FSharp.Collections.Seq.average"
            "Microsoft.FSharp.Collections.Seq.head"
            "Microsoft.FSharp.Collections.Seq.tryHead"
            "Microsoft.FSharp.Collections.Seq.last"
            "Microsoft.FSharp.Collections.Seq.tryLast"
            "Microsoft.FSharp.Collections.Seq.exactlyOne"
            "Microsoft.FSharp.Collections.Seq.tryExactlyOne"
            "Microsoft.FSharp.Collections.Seq.sum"
            "Microsoft.FSharp.Collections.Seq.averageBy"
            "Microsoft.FSharp.Collections.Seq.compareWith"
            "Microsoft.FSharp.Collections.Seq.countBy"
            "Microsoft.FSharp.Collections.Seq.exists2"
            "Microsoft.FSharp.Collections.Seq.find"
            "Microsoft.FSharp.Collections.Seq.findBack"
            "Microsoft.FSharp.Collections.Seq.findIndex"
            "Microsoft.FSharp.Collections.Seq.findIndexBack"
            "Microsoft.FSharp.Collections.Seq.fold2"
            "Microsoft.FSharp.Collections.Seq.foldBack2"
            "Microsoft.FSharp.Collections.Seq.forall2"
            "Microsoft.FSharp.Collections.Seq.item"
            "Microsoft.FSharp.Collections.Seq.iteri"
            "Microsoft.FSharp.Collections.Seq.iter2"
            "Microsoft.FSharp.Collections.Seq.iteri2"
            "Microsoft.FSharp.Collections.Seq.maxBy"
            "Microsoft.FSharp.Collections.Seq.minBy"
            "Microsoft.FSharp.Collections.Seq.pick"
            "Microsoft.FSharp.Collections.Seq.reduce"
            "Microsoft.FSharp.Collections.Seq.reduceBack"
            "Microsoft.FSharp.Collections.Seq.sumBy"
            "Microsoft.FSharp.Collections.Seq.tryFind"
            "Microsoft.FSharp.Collections.Seq.tryFindBack"
            "Microsoft.FSharp.Collections.Seq.tryFindIndex"
            "Microsoft.FSharp.Collections.Seq.tryFindIndexBack"
            "Microsoft.FSharp.Collections.Seq.tryItem"
            "Microsoft.FSharp.Collections.Seq.tryPick"
        ]

let constructFix (origRange : Range) (origSource : string) (fixedModuleName : string) =

    let fixText, lengthToReplace =
        if fixedModuleName = "Set" then // deal with the 3 differently named equivalents of Set
            if origSource.StartsWith ("Seq.length", StringComparison.Ordinal) then
                "Set.count", 10
            elif origSource.StartsWith ("Seq.max", StringComparison.Ordinal) then
                "Set.maxElement", 7
            elif origSource.StartsWith ("Seq.min", StringComparison.Ordinal) then
                "Set.minElement", 7
            else
                "Set", 3
        else
            fixedModuleName, 3

    let fixRange =
        Range.mkRange
            origRange.FileName
            origRange.Start
            (Position.mkPos origRange.StartLine (origRange.StartColumn + lengthToReplace))

    {
        FromRange = fixRange
        FromText = ""
        ToText = fixText
    }

let analyze (sourceText : ISourceText) (typedTree : FSharpImplementationFileContents) =
    let state = ResizeArray<string * string * range * Fix> ()

    let walker =
        { new TypedTreeCollectorBase() with
            override _.WalkCall _ (mfv : FSharpMemberOrFunctionOrValue) _ _ (args : FSharpExpr list) (range : range) =

                // check if we are calling a function in the Seq module that has an equivalent function in the (Array|List|Set) modules
                // check if all collection parameters were coerced from the same type (array|list|set) to seq
                // If yes, we could have used a function from the (Array|List|Set) modules

                let inAllCollections =
                    seqFuncsWithEquivalentsInAllCollections |> Set.contains mfv.FullName

                let inArrayAndList =
                    seqFuncsWithEquivalentsInArrayAndList |> Set.contains mfv.FullName

                if inAllCollections || inArrayAndList then
                    let seqParamIndexes =
                        mfv.CurriedParameterGroups
                        |> Seq.indexed
                        |> Seq.choose (fun (idx, g) ->
                            if
                                g.Count = 1
                                && g[0].Type.HasTypeDefinition
                                && g[0].Type.TypeDefinition.LogicalName = "seq`1"
                            then
                                Some idx
                            else
                                None
                        )
                        |> List.ofSeq

                    if not seqParamIndexes.IsEmpty then

                        let maxSeqParamIdx = List.last seqParamIndexes

                        if args.Length > maxSeqParamIdx then

                            let modules =
                                seqParamIndexes
                                |> List.choose (fun i ->
                                    match args[i] with
                                    | CoerceToSeq inAllCollections m -> Some m
                                    | _ -> None
                                )

                            if modules.Length = seqParamIndexes.Length && (List.distinct modules).Length = 1 then
                                let origSource = sourceText.GetContentAt range
                                let m = modules[0]
                                let fix = constructFix range origSource m
                                state.Add (mfv.DisplayName, m, range, fix)
        }

    walkTast walker typedTree

    [
        for seqFunc, valType, range, fix in state do
            {
                Type = "VirtualCall analyzer"
                Message =
                    $"Consider replacing the call of Seq.%s{seqFunc} with a function from the %s{valType} module to avoid the costs of virtual calls."
                Code = Code
                Severity = Warning
                Range = range
                Fixes = [ fix ]
            }
    ]

[<Literal>]
let Name = "VirtualCall Analyzer"

[<Literal>]
let ShortDescription =
    "Checks if calls of Seq functions can be replaced with functions from the collection modules"

[<Literal>]
let HelpUri =
    "https://g-research.github.io/fsharp-analyzers/analyzers/VirtualCallAnalyzer.html"

[<CliAnalyzer(Name, ShortDescription, HelpUri)>]
let virtualCallCliAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }

[<EditorAnalyzer(Name, ShortDescription, HelpUri)>]
let virtualCallEditorAnalyzer : Analyzer<EditorContext> =
    fun (ctx : EditorContext) ->
        async { return ctx.TypedTree |> Option.map (analyze ctx.SourceText) |> Option.defaultValue [] }
