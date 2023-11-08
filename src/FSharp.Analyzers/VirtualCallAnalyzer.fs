module GR.FSharp.Analyzers.VirtualCallAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Symbols.FSharpExprPatterns
open FSharp.Compiler.Text

[<Literal>]
let Code = "GRA-VIRTUALCALL-001"

let (|CoerceToSeq|_|) (includeFromSet : bool) (expr : FSharpExpr) =
    match expr with
    | Coerce (t, e) when
        t.HasTypeDefinition
        && t.TypeDefinition.LogicalName = "seq`1"
        && e.Type.HasTypeDefinition
        ->
        match e.Type.TypeDefinition.LogicalName with
        | "[]`1" -> Some "Array"
        | "list`1" -> Some "List"
        | "Set`1" when includeFromSet -> Some "Set"
        | _ -> None
    | _ -> None

[<CliAnalyzer>]
let virtualCallAnalyzer : Analyzer<CliContext> =
    fun (context : CliContext) ->
        async {
            let state = ResizeArray<string * string * range> ()

            let seqFuncsWithEquivalentsInAllCollections =
                set
                    [
                        "Microsoft.FSharp.Collections.Seq.isEmpty"
                        "Microsoft.FSharp.Collections.Seq.length" // ~ Set.count
                        "Microsoft.FSharp.Collections.Seq.max" // ~ Set.maxElement
                        "Microsoft.FSharp.Collections.Seq.min" // ~ Set.minElement
                        "Microsoft.FSharp.Collections.Seq.contains"
                        "Microsoft.FSharp.Collections.Seq.exists"
                        "Microsoft.FSharp.Collections.Seq.filter"
                        "Microsoft.FSharp.Collections.Seq.foldBack"
                        "Microsoft.FSharp.Collections.Seq.forall"
                        "Microsoft.FSharp.Collections.Seq.iter"
                        "Microsoft.FSharp.Collections.Seq.map"
                        "Microsoft.FSharp.Collections.Seq.fold"
                        "Microsoft.FSharp.Collections.Seq.except" // ~ Set.difference
                    ]

            let seqFuncsWithEquivalentsInArrayAndList =
                set
                    [
                        "Microsoft.FSharp.Collections.Seq.average"
                        "Microsoft.FSharp.Collections.Seq.distinct"
                        "Microsoft.FSharp.Collections.Seq.head"
                        "Microsoft.FSharp.Collections.Seq.tryHead"
                        "Microsoft.FSharp.Collections.Seq.last"
                        "Microsoft.FSharp.Collections.Seq.tryLast"
                        "Microsoft.FSharp.Collections.Seq.exactlyOne"
                        "Microsoft.FSharp.Collections.Seq.tryExactlyOne"
                        "Microsoft.FSharp.Collections.Seq.indexed"
                        "Microsoft.FSharp.Collections.Seq.pairwise"
                        "Microsoft.FSharp.Collections.Seq.rev"
                        "Microsoft.FSharp.Collections.Seq.sort"
                        "Microsoft.FSharp.Collections.Seq.sortDescending"
                        "Microsoft.FSharp.Collections.Seq.sum"
                        "Microsoft.FSharp.Collections.Seq.tail"
                        "Microsoft.FSharp.Collections.Seq.averageBy"
                        "Microsoft.FSharp.Collections.Seq.append"
                        "Microsoft.FSharp.Collections.Seq.choose"
                        "Microsoft.FSharp.Collections.Seq.chunkBySize"
                        "Microsoft.FSharp.Collections.Seq.collect"
                        "Microsoft.FSharp.Collections.Seq.compareWith"
                        "Microsoft.FSharp.Collections.Seq.concat"
                        "Microsoft.FSharp.Collections.Seq.countBy"
                        "Microsoft.FSharp.Collections.Seq.distinctBy"
                        "Microsoft.FSharp.Collections.Seq.splitInto"
                        "Microsoft.FSharp.Collections.Seq.exists2"
                        "Microsoft.FSharp.Collections.Seq.where"
                        "Microsoft.FSharp.Collections.Seq.find"
                        "Microsoft.FSharp.Collections.Seq.findBack"
                        "Microsoft.FSharp.Collections.Seq.findIndex"
                        "Microsoft.FSharp.Collections.Seq.findIndexBack"
                        "Microsoft.FSharp.Collections.Seq.fold2"
                        "Microsoft.FSharp.Collections.Seq.foldBack2"
                        "Microsoft.FSharp.Collections.Seq.forall2"
                        "Microsoft.FSharp.Collections.Seq.groupBy"
                        "Microsoft.FSharp.Collections.Seq.item"
                        "Microsoft.FSharp.Collections.Seq.iteri"
                        "Microsoft.FSharp.Collections.Seq.iter2"
                        "Microsoft.FSharp.Collections.Seq.iteri2"
                        "Microsoft.FSharp.Collections.Seq.map2"
                        "Microsoft.FSharp.Collections.Seq.mapFold"
                        "Microsoft.FSharp.Collections.Seq.mapFoldBack"
                        "Microsoft.FSharp.Collections.Seq.map3"
                        "Microsoft.FSharp.Collections.Seq.mapi"
                        "Microsoft.FSharp.Collections.Seq.mapi2"
                        "Microsoft.FSharp.Collections.Seq.maxBy"
                        "Microsoft.FSharp.Collections.Seq.minBy"
                        "Microsoft.FSharp.Collections.Seq.permute"
                        "Microsoft.FSharp.Collections.Seq.pick"
                        "Microsoft.FSharp.Collections.Seq.reduce"
                        "Microsoft.FSharp.Collections.Seq.replicate"
                        "Microsoft.FSharp.Collections.Seq.reduceBack"
                        "Microsoft.FSharp.Collections.Seq.scan"
                        "Microsoft.FSharp.Collections.Seq.scanBack"
                        "Microsoft.FSharp.Collections.Seq.skip"
                        "Microsoft.FSharp.Collections.Seq.skipWhile"
                        "Microsoft.FSharp.Collections.Seq.sortWith"
                        "Microsoft.FSharp.Collections.Seq.sortBy"
                        "Microsoft.FSharp.Collections.Seq.sortByDescending"
                        "Microsoft.FSharp.Collections.Seq.sumBy"
                        "Microsoft.FSharp.Collections.Seq.take"
                        "Microsoft.FSharp.Collections.Seq.takeWhile"
                        "Microsoft.FSharp.Collections.Seq.tryFind"
                        "Microsoft.FSharp.Collections.Seq.tryFindBack"
                        "Microsoft.FSharp.Collections.Seq.tryFindIndex"
                        "Microsoft.FSharp.Collections.Seq.tryFindIndexBack"
                        "Microsoft.FSharp.Collections.Seq.tryItem"
                        "Microsoft.FSharp.Collections.Seq.tryPick"
                        "Microsoft.FSharp.Collections.Seq.transpose"
                        "Microsoft.FSharp.Collections.Seq.truncate"
                        "Microsoft.FSharp.Collections.Seq.unfold"
                        "Microsoft.FSharp.Collections.Seq.windowed"
                        "Microsoft.FSharp.Collections.Seq.zip"
                        "Microsoft.FSharp.Collections.Seq.zip3"
                        "Microsoft.FSharp.Collections.Seq.removeAt"
                        "Microsoft.FSharp.Collections.Seq.removeManyAt"
                        "Microsoft.FSharp.Collections.Seq.updateAt"
                        "Microsoft.FSharp.Collections.Seq.insertAt"
                        "Microsoft.FSharp.Collections.Seq.insertManyAt"
                        "Microsoft.FSharp.Collections.Seq.allPairs"
                    ]

            let walker =
                { new TypedTreeCollectorBase() with
                    override _.WalkCall
                        _
                        (mfv : FSharpMemberOrFunctionOrValue)
                        _
                        _
                        (args : FSharpExpr list)
                        (range : range)
                        =

                        let inAllCollections =
                            seqFuncsWithEquivalentsInAllCollections |> Set.contains mfv.FullName

                        let inArrayAndList =
                            seqFuncsWithEquivalentsInArrayAndList |> Set.contains mfv.FullName

                        if inAllCollections || inArrayAndList then
                            let seqParamIndexes =
                                mfv.CurriedParameterGroups
                                |> Seq.indexed
                                |> Seq.filter (fun (_idx, g) ->
                                    g.Count = 1
                                    && g[0].Type.HasTypeDefinition
                                    && g[0].Type.TypeDefinition.LogicalName = "seq`1"
                                )
                                |> Seq.map fst
                                |> List.ofSeq

                            if not seqParamIndexes.IsEmpty then

                                let maxSeqParamIdx = Seq.last seqParamIndexes

                                if args.Length > maxSeqParamIdx then

                                    let modules =
                                        seqParamIndexes
                                        |> List.choose (fun i ->
                                            match args[i] with
                                            | CoerceToSeq inAllCollections m -> Some m
                                            | _ -> None
                                        )

                                    if
                                        modules.Length = seqParamIndexes.Length && (List.distinct modules).Length = 1
                                    then
                                        state.Add (mfv.DisplayName, modules[0], range)
                }

            match context.TypedTree with
            | Some t -> walkTast walker t
            | None -> ()

            return
                [
                    for seqFunc, valType, range in state do
                        {
                            Type = "VirtualCall analyzer"
                            Message =
                                $"Consider replacing the call of Seq.{seqFunc} with a function from the {valType} module to avoid the costs of virtual calls."
                            Code = Code
                            Severity = Warning
                            Range = range
                            Fixes = []
                        }
                ]
        }
