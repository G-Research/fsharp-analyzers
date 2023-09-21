namespace ``G-Research``.FSharp.Analyzers

open System
open FSharp.Analyzers.SDK
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols.FSharpExprPatterns

module JsonSerializerOptionsAnalyzer =

    [<Literal>]
    let Code = "GRA-002"

    let rec visitExpr memberCallHandler (e : FSharpExpr) =
        match e with
        | AddressOf (lvalueExpr) -> visitExpr memberCallHandler lvalueExpr
        | AddressSet (lvalueExpr, rvalueExpr) ->
            visitExpr memberCallHandler lvalueExpr
            visitExpr memberCallHandler rvalueExpr
        | Application (funcExpr, _typeArgs, argExprs) ->
            visitExpr memberCallHandler funcExpr
            visitExprs memberCallHandler argExprs
        | Call (objExprOpt, memberOrFunc, _typeArgs1, _typeArgs2, argExprs) -> // memberOrFunc = Deserialize
            memberCallHandler e.Range memberOrFunc argExprs
            visitObjArg memberCallHandler objExprOpt
            visitExprs memberCallHandler argExprs
        | Coerce (_targetType, inpExpr) -> visitExpr memberCallHandler inpExpr
        | FastIntegerForLoop (startExpr, limitExpr, consumeExpr, _isUp, _debugPointAtFor, _debugPointAtInOrTo) ->
            visitExpr memberCallHandler startExpr
            visitExpr memberCallHandler limitExpr
            visitExpr memberCallHandler consumeExpr
        | ILAsm (_asmCode, _typeArgs, argExprs) -> visitExprs memberCallHandler argExprs
        | ILFieldGet (objExprOpt, _fieldType, _fieldName) -> visitObjArg memberCallHandler objExprOpt
        | ILFieldSet (objExprOpt, _fieldType, _fieldName, _valueExpr) -> visitObjArg memberCallHandler objExprOpt
        | IfThenElse (guardExpr, thenExpr, elseExpr) ->
            visitExpr memberCallHandler guardExpr
            visitExpr memberCallHandler thenExpr
            visitExpr memberCallHandler elseExpr
        | Lambda (_lambdaVar, bodyExpr) -> visitExpr memberCallHandler bodyExpr
        | Let ((_bindingVar, bindingExpr, _debugPointAtBinding), bodyExpr) ->
            visitExpr memberCallHandler bindingExpr
            visitExpr memberCallHandler bodyExpr
        | LetRec (recursiveBindings, bodyExpr) ->
            let recursiveBindings' =
                recursiveBindings |> List.map (fun (mfv, expr, _dp) -> (mfv, expr))

            List.iter (snd >> visitExpr memberCallHandler) recursiveBindings'
            visitExpr memberCallHandler bodyExpr
        | NewArray (_arrayType, argExprs) -> visitExprs memberCallHandler argExprs
        | NewDelegate (_delegateType, delegateBodyExpr) -> visitExpr memberCallHandler delegateBodyExpr
        | NewObject (_objType, _typeArgs, argExprs) -> visitExprs memberCallHandler argExprs
        | NewRecord (_recordType, argExprs) -> visitExprs memberCallHandler argExprs
        | NewTuple (_tupleType, argExprs) -> visitExprs memberCallHandler argExprs
        | NewUnionCase (_unionType, _unionCase, argExprs) -> visitExprs memberCallHandler argExprs
        | Quote (quotedExpr) -> visitExpr memberCallHandler quotedExpr
        | FSharpFieldGet (objExprOpt, _recordOrClassType, _fieldInfo) -> visitObjArg memberCallHandler objExprOpt
        | FSharpFieldSet (objExprOpt, _recordOrClassType, _fieldInfo, argExpr) ->
            visitObjArg memberCallHandler objExprOpt
            visitExpr memberCallHandler argExpr
        | Sequential (firstExpr, secondExpr) ->
            visitExpr memberCallHandler firstExpr
            visitExpr memberCallHandler secondExpr
        | TryFinally (bodyExpr, finalizeExpr, _debugPointAtTry, _debugPointAtFinally) ->
            visitExpr memberCallHandler bodyExpr
            visitExpr memberCallHandler finalizeExpr
        | TryWith (bodyExpr, _, _, _catchVar, catchExpr, _debugPointAtTry, _debugPointAtWith) ->
            visitExpr memberCallHandler bodyExpr
            visitExpr memberCallHandler catchExpr
        | TupleGet (_tupleType, _tupleElemIndex, tupleExpr) -> visitExpr memberCallHandler tupleExpr
        | DecisionTree (decisionExpr, decisionTargets) ->
            visitExpr memberCallHandler decisionExpr
            List.iter (snd >> visitExpr memberCallHandler) decisionTargets
        | DecisionTreeSuccess (_decisionTargetIdx, decisionTargetExprs) ->
            visitExprs memberCallHandler decisionTargetExprs
        | TypeLambda (_genericParam, bodyExpr) -> visitExpr memberCallHandler bodyExpr
        | TypeTest (_ty, inpExpr) -> visitExpr memberCallHandler inpExpr
        | UnionCaseSet (unionExpr, _unionType, _unionCase, _unionCaseField, valueExpr) ->
            visitExpr memberCallHandler unionExpr
            visitExpr memberCallHandler valueExpr
        | UnionCaseGet (unionExpr, _unionType, _unionCase, _unionCaseField) -> visitExpr memberCallHandler unionExpr
        | UnionCaseTest (unionExpr, _unionType, _unionCase) -> visitExpr memberCallHandler unionExpr
        | UnionCaseTag (unionExpr, _unionType) -> visitExpr memberCallHandler unionExpr
        | ObjectExpr (_objType, baseCallExpr, overrides, interfaceImplementations) ->
            visitExpr memberCallHandler baseCallExpr
            List.iter (visitObjMember memberCallHandler) overrides
            List.iter (snd >> List.iter (visitObjMember memberCallHandler)) interfaceImplementations
        | TraitCall (_sourceTypes, _traitName, _typeArgs, _typeInstantiation, _argTypes, argExprs) ->
            visitExprs memberCallHandler argExprs
        | ValueSet (_valToSet, valueExpr) -> visitExpr memberCallHandler valueExpr
        | WhileLoop (guardExpr, bodyExpr, _debugPointAtWhile) ->
            visitExpr memberCallHandler guardExpr
            visitExpr memberCallHandler bodyExpr
        | BaseValue _baseType -> ()
        | DefaultValue _defaultType -> ()
        | ThisValue _thisType -> ()
        | Const (_constValueObj, _constType) -> ()
        | Value (_valueToGet) -> ()
        | _ -> ()

    and visitExprs f exprs = List.iter (visitExpr f) exprs

    and visitObjArg f objOpt = Option.iter (visitExpr f) objOpt

    and visitObjMember f memb = visitExpr f memb.Body

    let rec visitDeclaration f d =
        match d with
        | FSharpImplementationFileDeclaration.Entity (_e, subDecls) ->
            for subDecl in subDecls do
                visitDeclaration f subDecl
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (_v, _vs, e) -> visitExpr f e
        | FSharpImplementationFileDeclaration.InitAction (e) -> visitExpr f e

    [<CliAnalyzer "JsonSerializerOptionsAnalyzer">]
    let jsonSerializerOptionsAnalyzer : Analyzer<CliContext> =
        fun ctx ->
            async {
                let state = ResizeArray<range> ()

                let namesToWarnAbount =
                    [|
                        "System.Text.Json.JsonSerializer.Deserialize"
                        "System.Text.Json.JsonSerializer.DeserializeAsync"
                        "System.Text.Json.JsonSerializer.DeserializeAsyncEnumerable"
                        "System.Text.Json.JsonSerializer.Serialize"
                        "System.Text.Json.JsonSerializer.SerializeAsync"
                        "System.Text.Json.JsonSerializer.SerializeToDocument"
                        "System.Text.Json.JsonSerializer.SerializeToElement"
                        "System.Text.Json.JsonSerializer.SerializeToNode"
                        "System.Text.Json.JsonSerializer.SerializeToUtf8Bytes"
                    |]

                let handler (range : range) (m : FSharpMemberOrFunctionOrValue) (args : FSharpExpr list) =
                    let name = String.Join (".", m.DeclaringEntity.Value.FullName, m.DisplayName)

                    let containsSerOptsCtorCall =
                        args
                        |> List.exists (
                            function
                            | NewObject (objType, _, _) when
                                objType.FullName = "System.Text.Json.JsonSerializerOptions"
                                && objType.Assembly.SimpleName = "System.Text.Json"
                                ->
                                true
                            | _ -> false
                        )

                    if
                        m.Assembly.SimpleName = "System.Text.Json"
                        && Array.contains name namesToWarnAbount
                        && containsSerOptsCtorCall
                    then
                        state.Add range

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
