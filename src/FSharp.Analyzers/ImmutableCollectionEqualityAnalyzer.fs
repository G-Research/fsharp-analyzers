module GR.FSharp.Analyzers.ImmutableCollectionEqualityAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.TASTCollecting
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns


[<Literal>]
let Code = "GRA-IMMUTABLECOLLECTIONEQUALITY-001"

let isImmutableType (basicQualifiedName : string) (e : FSharpExpr) =
    let e =
        match e with
        | Coerce (_, e)
        | e -> e

    let t = e.Type

    t.BasicQualifiedName = basicQualifiedName
    && t.TypeDefinition.Assembly.SimpleName = "System.Collections.Immutable"

let isImmutableDictionaryType (e : FSharpExpr) =
    isImmutableType "System.Collections.Immutable.ImmutableDictionary`2" e

let isImmutableHashSetType (e : FSharpExpr) =
    isImmutableType "System.Collections.Immutable.ImmutableHashSet`1" e

let mkMessage typeName m =
    {
        Type = "ImmutableCollectionEqualityAnalyzer"
        Message =
            $"%s{typeName} does not implement structural equality. Use Object.ReferenceEquals instead .Equals, = or <> to emphasize this."
        Code = Code
        Severity = Warning
        Range = m
        Fixes = []
    }

[<CliAnalyzer("ImmutableCollectionEqualityAnalyzer",
              "TODO: add description.",
              "https://g-research.github.io/fsharp-analyzers/analyzers/ImmutableCollectionEqualityAnalyzer.html")>]
let immutableCollectionEqualityAnalyzer : Analyzer<CliContext> =
    fun (ctx : CliContext) ->
        async {

            let invocations = ResizeArray<Message> ()

            let walker =
                { new TypedTreeCollectorBase() with
                    override _.WalkCall
                        (objExprOpt : FSharpExpr option)
                        (mfv : FSharpMemberOrFunctionOrValue)
                        _
                        _
                        (args : FSharpExpr list)
                        (m : range)
                        =
                        let xsOpt =
                            if
                                (mfv.FullName = "System.Object.Equals")
                                && (mfv.Assembly.SimpleName = "System.Runtime"
                                    || mfv.Assembly.SimpleName = "netstandard")
                                && args.Length = 1
                            then
                                objExprOpt |> Option.map (fun objExpr -> args.[0], objExpr)
                            elif
                                (mfv.FullName = "Microsoft.FSharp.Core.Operators.(=)"
                                 || mfv.FullName = "Microsoft.FSharp.Core.Operators.(<>)")
                                && mfv.Assembly.SimpleName = "FSharp.Core"
                                && args.Length = 2
                            then
                                Some (args.[0], args.[1])
                            else
                                None

                        match xsOpt with
                        | None -> ()
                        | Some (e1, e2) ->

                        if isImmutableDictionaryType e1 && isImmutableDictionaryType e2 then
                            invocations.Add (mkMessage "System.Collections.Immutable.ImmutableDictionary" m)
                        elif isImmutableHashSetType e1 && isImmutableHashSetType e2 then
                            invocations.Add (mkMessage "System.Collections.Immutable.ImmutableHashSet" m)
                }

            match ctx.TypedTree with
            | None -> return List.empty<Message>
            | Some typedTree ->
                walkTast walker typedTree
                return Seq.toList invocations
        }
