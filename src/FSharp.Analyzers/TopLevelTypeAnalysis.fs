module TopLevelTypeAnalysis

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

#nowarn "1182"

type MissingReturnType =
    {
        /// Return type name formatted using proper DisplayContext.
        TypeName : string
        /// Range of the `=` in binding.
        Equals : range
    }

type MissingParameterType =
    {
        /// Return type name formatted using proper DisplayContext.
        TypeName : string
        /// Range of the parameter pattern
        Range : range
        /// Friendly name of the parameter. Might not be present.
        ParameterName : string option
        /// Index in the function signature.
        Index : int
        /// Indicates if the current pattern requires to be wrapped in additional parentheses.
        AddParentheses : bool
    }

type MissingGenericParameterInfo =
    {
        /// Name of generic parameter
        Name : string
        /// Missing untyped constraints in the format: `'a : equality`
        /// It could be the case that this list is empty and the generic parameter was not present in the source text.
        MissingConstraints : string list
    }

[<RequireQualifiedAccess>]
type Declaration =
    | Binding of
        /// The last ident of the binding name.
        name : Ident *
        /// Let keyword range
        leadingKeyword : SynLeadingKeyword *
        returnType : MissingReturnType option
    | ImplicitCtor of
        /// Friendly name of the type.
        typeName : LongIdent *
        /// Constructor arguments range, does include parentheses.
        simplePatsRange : range

/// Missing information for a binding to extract signature data.
type MissingTypeInfo =
    {
        Declaration : Declaration
        Parameters : MissingParameterType list
        /// Missing generic parameters
        /// If a generic parameter is missing a constraint it won't
        GenericParameters : MissingGenericParameterInfo list
        /// Were any symbol uses found inside the project for this value?
        /// It might still be used outside the project.
        ValueIsUsedOutsideTheFileInTheProject : bool
    }

type FSharpGenericParameter with
    member private gp.HasEqualityConstraint =
        gp.Constraints |> Seq.exists (fun c -> c.IsEqualityConstraint)

type private Env =
    {
        SourceText : ISourceText
        CheckFileResults : FSharpCheckFileResults
        CheckProjectResults : FSharpCheckProjectResults
    }

/// Type that bridges untyped tree with FSharpGenericParameterConstraint
/// See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-symbols-fsharpgenericparameterconstraint.html
type private UntypedGenericParameterInfo =
    {
        IsEqualityConstraint : bool
    }

    static member Empty = { IsEqualityConstraint = false }

let rec private removePatternParens (p : SynPat) =
    match p with
    | SynPat.Paren (pat = inner) -> removePatternParens inner
    | _ -> p

let private symbolHasUsesOutsideFile (env : Env) (symbolUse : FSharpSymbolUse) =
    env.CheckProjectResults.GetUsesOfSymbol symbolUse.Symbol
    |> Array.exists (fun symbolUse -> symbolUse.FileName <> symbolUse.FileName)

let private findMissingGenericParameterInfos
    (symbolUse : FSharpSymbolUse)
    (typarDecls : SynTyparDecls option)
    : MissingGenericParameterInfo list
    =
    let untypedConstraints : Map<string, UntypedGenericParameterInfo> =
        match typarDecls with
        | None -> Map.empty
        | Some typarDecls ->

        match typarDecls with
        | SynTyparDecls.PostfixList (decls = decls ; constraints = constraints) ->
            let initialMap =
                decls
                |> List.map (fun (SynTyparDecl (typar = SynTypar (ident = ident))) ->
                    ident.idText, UntypedGenericParameterInfo.Empty
                )
                |> Map.ofList

            (initialMap, constraints)
            ||> List.fold (fun map stc ->
                match stc with
                | SynTypeConstraint.WhereTyparIsValueType (typar, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsValueType"
                | SynTypeConstraint.WhereTyparIsReferenceType (typar, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsReferenceType"
                | SynTypeConstraint.WhereTyparIsUnmanaged (typar, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsUnmanaged"
                | SynTypeConstraint.WhereTyparSupportsNull (typar, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparSupportsNull"
                | SynTypeConstraint.WhereTyparIsComparable (typar, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsComparable"
                | SynTypeConstraint.WhereTyparIsEquatable ((SynTypar (ident = ident)), range) ->
                    let info =
                        Map.tryFind ident.idText map
                        |> Option.defaultValue UntypedGenericParameterInfo.Empty

                    Map.add ident.idText { IsEqualityConstraint = true } map
                | SynTypeConstraint.WhereTyparDefaultsToType (typar, typeName, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparDefaultsToType"
                | SynTypeConstraint.WhereTyparSubtypeOfType (typar, typeName, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparSubtypeOfType"
                | SynTypeConstraint.WhereTyparSupportsMember (typars, memberSig, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparSupportsMember"
                | SynTypeConstraint.WhereTyparIsEnum (typar, typeArgs, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsEnum"
                | SynTypeConstraint.WhereTyparIsDelegate (typar, typeArgs, range) ->
                    failwith "todo: SynTypeConstraint.WhereTyparIsDelegate"
                | SynTypeConstraint.WhereSelfConstrained (selfConstraint, range) ->
                    failwith "todo: SynTypeConstraint.WhereSelfConstrained"
            )
        | SynTyparDecls.PrefixList _ -> failwith "todo: SynTyparDecls.PrefixList"
        | SynTyparDecls.SinglePrefix _ -> failwith "todo: SynTyparDecls.SinglePrefix"

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.GenericParameters
        |> Seq.choose (fun gp ->
            let missingConstraints =
                if Seq.isEmpty gp.Constraints then
                    List.empty
                else
                    // Check if each constraint was found in the source
                    match Map.tryFind gp.Name untypedConstraints with
                    | None ->
                        [
                            if gp.HasEqualityConstraint then
                                yield $"%s{gp.Name} : equality"
                        ]
                    | Some untypedConstraint ->
                        [
                            if untypedConstraint.IsEqualityConstraint <> gp.HasEqualityConstraint then
                                yield $"%s{gp.Name} : equality"
                        ]

            match missingConstraints with
            | [] ->
                if Map.containsKey gp.Name untypedConstraints then
                    None
                else
                    Some
                        {
                            Name = gp.Name
                            MissingConstraints = []
                        }
            | missingConstraints ->
                Some
                    {
                        Name = gp.Name
                        MissingConstraints = missingConstraints
                    }
        )
        |> Seq.toList
    | _ -> []

let private processBinding
    (env : Env)
    (SynBinding (headPat = headPat ; returnInfo = returnInfo ; trivia = trivia))
    : MissingTypeInfo option
    =
    let {
            SourceText = sourceText
            CheckFileResults = checkFileResults
            CheckProjectResults = checkProjectResults
        } =
        env

    let isPrivate =
        match headPat with
        | SynPat.Named (accessibility = Some (SynAccess.Private _))
        | SynPat.LongIdent (accessibility = Some (SynAccess.Private _)) -> true
        | _ -> false

    let ident =
        match headPat with
        | SynPat.Named (ident = SynIdent (ident = ident)) -> ident
        | SynPat.LongIdent (longDotId = sli) ->
            sli.LongIdent
            |> List.tryLast
            |> Option.defaultWith (fun () -> failwithf $"Could not find a name for %A{headPat.Range}")
        | _ -> failwithf $"Could not find a name for %A{headPat.Range}"

    let symbol, mfv =
        let line = sourceText.GetLineString (ident.idRange.EndLine - 1)

        checkFileResults.GetSymbolUseAtLocation (ident.idRange.EndLine, ident.idRange.EndColumn, line, [ ident.idText ])
        |> Option.bind (fun symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv -> Some (symbolUse, mfv)
            | _ -> None
        )
        |> Option.defaultWith (fun () -> failwithf $"Could not find symbol for %s{ident.idText}")

    let hasUsesOutsideOfFile = symbolHasUsesOutsideFile env symbol

    // Does every parameter has a fully type?
    let untypedParameters =
        match headPat with
        | SynPat.LongIdent (argPats = SynArgPats.Pats (pats = pats)) ->
            pats
            |> List.indexed
            |> List.choose (fun (idx, pat) ->
                let pat = removePatternParens pat

                match pat with
                | SynPat.Typed (targetType = targetType) ->
                    // TODO: the type might be incomplete, if it has wildcard for example.
                    None
                | untypedPat ->
                    match Seq.tryItem idx mfv.CurriedParameterGroups with
                    | None ->
                        failwith
                            $"There is no curried parameter group for this untyped parameter (%i{idx}) in %s{ident.idText}"
                    | Some pg ->
                        if pg.Count = 1 then
                            let singleParameter = pg.[0]

                            let parameterName =
                                match untypedPat with
                                | SynPat.Named (ident = SynIdent (ident = ident)) -> Some ident.idText
                                | _ -> None

                            Some
                                {
                                    Index = idx
                                    TypeName = singleParameter.Type.Format (symbol.DisplayContext)
                                    Range = untypedPat.Range
                                    ParameterName = parameterName
                                    AddParentheses = Option.isNone parameterName
                                }
                        else
                            let tupleType =
                                // TODO: maybe wrap in parentheses to be on the safe side?
                                pg
                                |> Seq.map (fun p -> p.Type.Format (symbol.DisplayContext))
                                |> String.concat " * "

                            Some
                                {
                                    Index = idx
                                    ParameterName = None
                                    TypeName = tupleType
                                    Range = untypedPat.Range
                                    AddParentheses = true
                                }
            )
        | _ ->
            // There were no parameters
            []

    let untypedParameterCount =
        match headPat with
        | SynPat.LongIdent (argPats = SynArgPats.Pats (pats = pats)) -> pats.Length
        | _ -> 0

    // Is there a return type?
    let missingReturnType =
        match returnInfo with
        | None ->
            let returnTypeText =
                if not mfv.FullType.IsFunctionType then
                    mfv.ReturnParameter.Type.Format symbol.DisplayContext
                else
                    // We can't really be trust mfv.ReturnParameter, it will only contain the last type in a function type.
                    // Instead we collect all types and skip the amount of parameters we have in the function definition.
                    let allTypesFromFunctionType : FSharpType list =
                        let rec visit (t : FSharpType) (continuation : FSharpType list -> FSharpType list) =
                            if not t.IsFunctionType then
                                continuation [ t ]
                            else
                                let funcType = t.GenericArguments.[0]
                                let argType = t.GenericArguments.[1]

                                if not argType.IsFunctionType then
                                    continuation [ funcType ; argType ]
                                else
                                    visit argType (fun types -> funcType :: types |> continuation)

                        visit mfv.FullType id

                    if allTypesFromFunctionType.Length <= untypedParameterCount then
                        mfv.ReturnParameter.Type.Format (symbol.DisplayContext)
                    else
                        allTypesFromFunctionType
                        |> List.skip untypedParameterCount
                        |> List.map (fun t ->
                            let formattedType = t.Format symbol.DisplayContext

                            if t.IsFunctionType then
                                $"({formattedType})"
                            else
                                formattedType
                        )
                        |> String.concat " -> "

            Some
                {
                    TypeName = returnTypeText
                    Equals =
                        trivia.EqualsRange
                        |> Option.defaultWith (fun () -> failwith $"No equals sign in binding %s{ident.idText}")
                }

        // TODO: again incomplete, type cannot have wildcards.
        | Some _ -> None

    // Are there inferred constraints not in the source.
    let missingGenericParameterInfos =
        match headPat with
        | SynPat.LongIdent (typarDecls = typarDecls) ->
            let typeParams =
                typarDecls
                |> Option.bind (fun (SynValTyparDecls (typars = typarDecls)) -> typarDecls)

            findMissingGenericParameterInfos symbol typeParams
        | _ -> []

    // Are all potential inferred constraints present?
    let isNotMissingInformation =
        isPrivate
        || (List.isEmpty untypedParameters
            && Option.isNone missingReturnType
            && List.isEmpty missingGenericParameterInfos)

    if isNotMissingInformation then
        None
    else
        Some
            {
                Declaration = Declaration.Binding (ident, trivia.LeadingKeyword, missingReturnType)
                Parameters = untypedParameters
                GenericParameters = missingGenericParameterInfos
                ValueIsUsedOutsideTheFileInTheProject = hasUsesOutsideOfFile
            }

let private processSimplePats (env : Env) (SynSimplePats.SimplePats (pats = pats)) =
    pats
    |> List.indexed
    |> List.choose (fun (idx, simplePat) ->
        match simplePat with
        | SynSimplePat.Id (ident = ident) ->
            let line = env.SourceText.GetLineString (ident.idRange.EndLine - 1)

            env.CheckFileResults.GetSymbolUseAtLocation (
                ident.idRange.EndLine,
                ident.idRange.EndColumn,
                line,
                [ ident.idText ]
            )
            |> Option.bind (fun symbolUse ->
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv ->
                    Some
                        {
                            TypeName = mfv.FullType.Format (symbolUse.DisplayContext)
                            Range = ident.idRange
                            Index = idx
                            ParameterName = Some ident.idText
                            AddParentheses = false
                        }
                | _ -> None
            )
        // TODO: what was the deal again with attributes?
        | _ -> None
    )

let private processMember
    (typeName : LongIdent)
    (typeParams : SynTyparDecls option)
    (env : Env)
    (md : SynMemberDefn)
    : MissingTypeInfo list
    =
    match md with
    | SynMemberDefn.NestedType _
    | SynMemberDefn.LetBindings _
    | SynMemberDefn.Open _ -> []
    | SynMemberDefn.Member (memberDefn = memberDefn) -> processBinding env memberDefn |> Option.toList
    | SynMemberDefn.GetSetMember (memberDefnForGet, memberDefnForSet, range, trivia) ->
        failwith "todo: SynMemberDefn.GetSetMember"
    | SynMemberDefn.ImplicitCtor (accessibility, attributes, ctorArgs, selfIdentifier, xmlDoc, range, trivia) ->
        match accessibility with
        | Some (SynAccess.Private _) -> []
        | _ ->
            typeName
            |> List.tryLast
            |> Option.bind (fun ident ->
                let lineText = env.SourceText.GetLineString (ident.idRange.EndLine - 1)

                env.CheckFileResults.GetSymbolUseAtLocation (
                    ident.idRange.EndLine,
                    ident.idRange.EndColumn,
                    lineText,
                    [ ".ctor" ]
                )
            )
            |> Option.bind (fun symbolUse ->
                let valueIsUsedOutsideTheFileInTheProject = symbolHasUsesOutsideFile env symbolUse
                let parameters = processSimplePats env ctorArgs
                let genericParameters = findMissingGenericParameterInfos symbolUse typeParams

                if List.isEmpty parameters && List.isEmpty genericParameters then
                    None
                else
                    Some
                        {
                            Declaration = Declaration.ImplicitCtor (typeName, ctorArgs.Range)
                            Parameters = parameters
                            GenericParameters = genericParameters
                            ValueIsUsedOutsideTheFileInTheProject = valueIsUsedOutsideTheFileInTheProject
                        }
            )
            |> Option.toList
    | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, inheritAlias, range) ->
        failwith "todo: SynMemberDefn.ImplicitInherit"
    | SynMemberDefn.AbstractSlot (slotSig, flags, range, trivia) -> failwith "todo: SynMemberDefn.AbstractSlot"
    | SynMemberDefn.Interface (interfaceType, withKeyword, members, range) -> failwith "todo: SynMemberDefn.Interface"
    | SynMemberDefn.Inherit (baseType, asIdent, range) -> failwith "todo: SynMemberDefn.Inherit"
    | SynMemberDefn.ValField (fieldInfo, range) -> failwith "todo: SynMemberDefn.ValField"
    | SynMemberDefn.AutoProperty (attributes,
                                  isStatic,
                                  ident,
                                  typeOpt,
                                  propKind,
                                  memberFlags,
                                  memberFlagsForSet,
                                  xmlDoc,
                                  accessibility,
                                  synExpr,
                                  range,
                                  trivia) -> failwith "todo: SynMemberDefn.AutoProperty"

let private processModuleDecl (env : Env) (mdl : SynModuleDecl) =
    match mdl with
    | SynModuleDecl.Let (bindings = bindings) -> List.choose (processBinding env) bindings
    | SynModuleDecl.Types (typeDefns = typeDefns) ->
        [
            for SynTypeDefn (typeRepr = typeRepr ; members = additionalMembers ; typeInfo = typeInfo) in typeDefns do
                let (SynComponentInfo (longId = typeName ; typeParams = typeParams)) = typeInfo

                match typeRepr with
                | SynTypeDefnRepr.Simple _
                | SynTypeDefnRepr.Exception _ -> ()
                | SynTypeDefnRepr.ObjectModel (members = members) ->
                    yield! List.collect (processMember typeName typeParams env) members

                yield! List.collect (processMember typeName typeParams env) additionalMembers
        ]
    | _ -> []

let private processModuleOrNamespace (env : Env) (SynModuleOrNamespace (decls = decls)) =
    List.collect (processModuleDecl env) decls

let findMissingTypeInformation
    (sourceText : ISourceText)
    (ast : ParsedInput)
    (checkFileResults : FSharpCheckFileResults)
    (checkProjectResults : FSharpCheckProjectResults)
    : MissingTypeInfo list
    =
    match ast with
    | ParsedInput.SigFile _ -> []
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (contents = modules)) ->
        let env =
            {
                SourceText = sourceText
                CheckFileResults = checkFileResults
                CheckProjectResults = checkProjectResults
            }

        modules |> List.collect (processModuleOrNamespace env)
