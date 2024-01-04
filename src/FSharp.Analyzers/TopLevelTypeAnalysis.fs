module TopLevelTypeAnalysis

open System.Text
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

type MissingParameterTypeInfo =
    {
        /// Return type name formatted using proper DisplayContext.
        TypeName : string
        /// Source text of the parameter pattern
        SourceText : string
        /// Range of the parameter pattern
        Range : range
        /// Friendly name of the parameter.
        ParameterName : string
        /// Used in a type constructor
        InsideConstructor : bool
    }

type MissingParameterPatternTypeInfo =
    {
        /// Return type name formatted using proper DisplayContext.
        TypeName : string
        /// Source text of the parameter pattern
        SourceText : string
        /// Range of the parameter pattern
        Range : range
        /// Index in the untyped tree
        Index : int
    }

[<RequireQualifiedAccess>]
type MissingParameterType =
    | SingleParameter of info : MissingParameterTypeInfo
    | SimpleTupleParameter of items : MissingParameterTypeInfo list
    | Pattern of info : MissingParameterPatternTypeInfo

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
        inlineKeyword : range option *
        returnType : MissingReturnType option
    | ImplicitCtor of
        /// The last ident of the type name.
        typeName : Ident *
        /// Constructor arguments range, does include parentheses.
        simplePatsRange : range
    | AutoProperty of
        name : Ident *
        /// Return type name formatted using proper DisplayContext.
        TypeName : string

/// Missing information for a binding to extract signature data.
type MissingTypeInfo =
    {
        Declaration : Declaration
        Parameters : MissingParameterType list
        /// Missing generic parameters
        /// If a generic parameter is missing a constraint it won't
        GenericParameters : MissingGenericParameterInfo list
        /// Were any symbol uses found inside the project for this value?
        /// This only applies to top level let bindings.
        /// It might still be used outside the project.
        ValueCouldBeMadePrivateToFile : bool
        /// The current generic parameter definition in the untyped tree.
        CurrentGenericParameters : SynTyparDecls option
    }

type FSharpGenericParameter with

    member private gp.HasEqualityConstraint =
        gp.Constraints |> Seq.exists (fun c -> c.IsEqualityConstraint)

    member private gp.NameWithTick =
        if gp.IsSolveAtCompileTime then
            $"^%s{gp.Name}"
        else
            $"'%s{gp.Name}"

    member private gp.SubtypesOfTypeConstraint (dp : FSharpDisplayContext) =
        gp.Constraints
        |> Seq.choose (fun c ->
            if not c.IsCoercesToConstraint then
                None
            else
                let t = c.CoercesToTarget.Format dp
                Some $"%s{gp.NameWithTick} :> %s{t}"
        )
        |> Seq.toList

type ISourceText with

    member private x.GetContentAt (range : range) : string =
        let startLine = range.StartLine - 1
        let line = x.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring (range.StartColumn, length)
        else
            let firstLineContent = line.Substring range.StartColumn
            let sb = StringBuilder().AppendLine firstLineContent

            (sb, [ range.StartLine .. range.EndLine - 2 ])
            ||> List.fold (fun sb lineNumber -> sb.AppendLine (x.GetLineString lineNumber))
            |> fun sb ->
                let lastLine = x.GetLineString (range.EndLine - 1)

                sb.Append(lastLine.Substring (0, range.EndColumn)).ToString ()

type private Env =
    {
        SourceText : ISourceText
        CheckFileResults : FSharpCheckFileResults
        CheckProjectResults : FSharpCheckProjectResults
        InsideNestedModule : bool
    }

/// Type that bridges untyped tree with FSharpGenericParameterConstraint
/// See https://fsharp.github.io/fsharp-compiler-docs/reference/fsharp-compiler-symbols-fsharpgenericparameterconstraint.html
type private UntypedGenericParameterInfo =
    {
        IsEqualityConstraint : bool
        SubtypesOfType : string list
    }

    static member Empty =
        {
            IsEqualityConstraint = false
            SubtypesOfType = []
        }

let rec private removePatternParens (p : SynPat) =
    match p with
    | SynPat.Paren (pat = inner) -> removePatternParens inner
    | _ -> p

let (|Mfv|_|) (symbolUse : FSharpSymbolUse) =
    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv -> Some mfv
    | _ -> None

let private valueCouldBeMadePrivateToFile (env : Env) (symbolUse : FSharpSymbolUse) =
    match symbolUse with
    // Active patterns can't be detected it seems.
    | Mfv mfv when mfv.IsActivePattern -> true
    | _ ->

    if env.InsideNestedModule then
        false
    else

    env.CheckProjectResults.GetUsesOfSymbol symbolUse.Symbol
    |> Array.forall (fun su -> su.FileName = symbolUse.FileName)

type SynTypar with
    member x.Name : string =
        let (SynTypar (ident = ident ; staticReq = sr)) = x

        match sr with
        | TyparStaticReq.HeadType -> $"^%s{ident.idText}"
        | TyparStaticReq.None -> $"'%s{ident.idText}"

let private findMissingGenericParameterInfos
    (env : Env)
    (symbolUse : FSharpSymbolUse)
    (typarDecls : SynTyparDecls option)
    : MissingGenericParameterInfo list
    =
    match symbolUse with
    | Mfv mfv when mfv.IsPropertyGetterMethod -> []
    | _ ->

    let untypedConstraints : Map<string, UntypedGenericParameterInfo> =
        match typarDecls with
        | None -> Map.empty
        | Some typarDecls ->

        match typarDecls with
        | SynTyparDecls.PostfixList (decls = decls ; constraints = constraints) ->
            let typars =
                decls
                |> List.map (fun (SynTyparDecl (typar = typar)) -> typar.Name, UntypedGenericParameterInfo.Empty)
                |> Map.ofList

            (typars, constraints)
            ||> List.fold (fun map stc ->
                match stc with
                | SynTypeConstraint.WhereTyparIsValueType (typar, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsValueType %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsReferenceType (typar, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsReferenceType %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsUnmanaged (typar, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsUnmanaged %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparSupportsNull (typar, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparSupportsNull %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsComparable (typar, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsComparable %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsEquatable (typar, range) ->
                    let typarInfo =
                        Map.tryFind typar.Name map
                        |> Option.defaultValue UntypedGenericParameterInfo.Empty

                    Map.add
                        typar.Name
                        { typarInfo with
                            IsEqualityConstraint = true
                        }
                        map
                | SynTypeConstraint.WhereTyparDefaultsToType (typar, typeName, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparDefaultsToType %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparSubtypeOfType (typar, typeName, range) ->
                    let typarInfo =
                        Map.tryFind typar.Name map
                        |> Option.defaultValue UntypedGenericParameterInfo.Empty

                    let typeText = env.SourceText.GetContentAt typeName.Range

                    Map.add
                        typar.Name
                        { typarInfo with
                            SubtypesOfType = typeText :: typarInfo.SubtypesOfType
                        }
                        map
                | SynTypeConstraint.WhereTyparSupportsMember (typars, memberSig, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparSupportsMember %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsEnum (typar, typeArgs, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsEnum %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparIsDelegate (typar, typeArgs, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparIsDelegate %A %s" range range.FileName
                | SynTypeConstraint.WhereSelfConstrained (selfConstraint, range) ->
                    failwithf "todo: SynTypeConstraint.WhereSelfConstrained %A %s" range range.FileName
            )
        | SynTyparDecls.PrefixList (range = range) ->
            failwithf "todo: SynTyparDecls.PrefixList %A %s" range range.FileName
        | SynTyparDecls.SinglePrefix (range = range) ->
            failwithf "todo: SynTyparDecls.SinglePrefix %A %s" range range.FileName

    match symbolUse.Symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.GenericParameters
        |> Seq.choose (fun gp ->
            let missingConstraints =
                if Seq.isEmpty gp.Constraints then
                    List.empty
                else
                    // Check if each constraint was found in the source
                    match Map.tryFind gp.NameWithTick untypedConstraints with
                    | None ->
                        let subtypeConstraints = gp.SubtypesOfTypeConstraint symbolUse.DisplayContext

                        [
                            if gp.HasEqualityConstraint then
                                yield $"%s{gp.NameWithTick} : equality"
                            yield! subtypeConstraints
                        ]
                    | Some untypedConstraint ->
                        let subtypeConstraints = gp.SubtypesOfTypeConstraint symbolUse.DisplayContext

                        [
                            if untypedConstraint.IsEqualityConstraint <> gp.HasEqualityConstraint then
                                yield $"%s{gp.NameWithTick} : equality"
                            if untypedConstraint.SubtypesOfType.Length <> subtypeConstraints.Length then
                                yield! subtypeConstraints
                        ]

            match missingConstraints with
            | [] ->
                if Map.containsKey gp.NameWithTick untypedConstraints then
                    None
                else
                    Some
                        {
                            Name = gp.NameWithTick
                            MissingConstraints = []
                        }
            | missingConstraints ->
                Some
                    {
                        Name = gp.NameWithTick
                        MissingConstraints = missingConstraints
                    }
        )
        |> Seq.toList
    | _ -> []

let rec private hasWildCardInSynType (t : SynType) : bool =
    match t with
    | SynType.Anon _ -> true
    | SynType.LongIdentApp (typeName = typeName ; typeArgs = typeArgs)
    | SynType.App (typeName = typeName ; typeArgs = typeArgs) ->
        hasWildCardInSynType typeName || List.exists hasWildCardInSynType typeArgs
    | SynType.Array (elementType = t) -> hasWildCardInSynType t
    | SynType.Fun (argType = t ; returnType = rt) -> hasWildCardInSynType t || hasWildCardInSynType rt
    | SynType.Intersection (types = ts) -> List.exists hasWildCardInSynType ts
    | SynType.Or (lhsType = lhs ; rhsType = rhs) -> hasWildCardInSynType lhs || hasWildCardInSynType rhs
    | SynType.Paren (innerType = t) -> hasWildCardInSynType t
    | SynType.Tuple (path = path) ->
        path
        |> List.exists (
            function
            | SynTupleTypeSegment.Slash _
            | SynTupleTypeSegment.Star _ -> false
            | SynTupleTypeSegment.Type t -> hasWildCardInSynType t
        )
    | SynType.Var _ -> false
    | SynType.AnonRecd (fields = fields) -> fields |> List.exists (snd >> hasWildCardInSynType)
    | SynType.HashConstraint (innerType = t) -> hasWildCardInSynType t
    | SynType.LongIdent _ -> false
    | SynType.MeasurePower (baseMeasure = t) -> hasWildCardInSynType t
    | SynType.SignatureParameter (usedType = t) -> hasWildCardInSynType t
    | SynType.StaticConstant _
    | SynType.FromParseError _
    | SynType.StaticConstantExpr _ -> false
    | SynType.StaticConstantNamed (value = t)
    | SynType.WithGlobalConstraints (typeName = t) -> hasWildCardInSynType t

let (|TuplePatWithUntyped|_|) (p : SynPat) =
    match p with
    | SynPat.Tuple (elementPats = elements) ->
        let totalCount = elements.Length

        let untypedParameters =
            elements
            |> List.indexed
            |> List.choose (fun (idx, pat) ->
                match pat with
                | SynPat.Typed (targetType = t ; pat = SynPat.Named (ident = SynIdent (ident = ident))) when
                    hasWildCardInSynType t
                    ->
                    Some (idx, ident.idText, ident.idRange, false)
                | SynPat.Named (ident = SynIdent (ident = ident)) -> Some (idx, ident.idText, ident.idRange, false)
                | SynPat.OptionalVal (ident = ident ; range = m) -> Some (idx, ident.idText, m, true)
                | _ -> None
            )

        if untypedParameters.IsEmpty then
            None
        else
            Some (totalCount, untypedParameters)
    | _ -> None

let private (|TypedPatWithoutWildcards|_|) (p : SynPat) =
    match p with
    | SynPat.Typed (targetType = t) when not (hasWildCardInSynType t) -> Some p
    | _ -> None

let private allTypedPatterns (pats : SynPat list) : bool =
    pats
    |> List.forall (
        function
        | SynPat.Typed (targetType = t) -> not (hasWildCardInSynType t)
        | _ -> false
    )

let private mkReturnType
    (untypedParameterCount : int)
    (symbolUse : FSharpSymbolUse)
    (mfv : FSharpMemberOrFunctionOrValue)
    =
    if not mfv.FullType.IsFunctionType then
        mfv.ReturnParameter.Type.Format symbolUse.DisplayContext
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
            mfv.ReturnParameter.Type.Format symbolUse.DisplayContext
        else
            let skip =
                // In case of a value member like  `member this.V = 4`
                // The return type will be: `OwnerType -> unit -> value`
                if mfv.IsMember && allTypesFromFunctionType.Length = 3 && untypedParameterCount = 0 then
                    2
                // member this.F (x:int) = 5 - x
                elif mfv.IsInstanceMember then
                    1 + untypedParameterCount
                // static member V = 5
                elif mfv.IsMember && not mfv.IsInstanceMember && untypedParameterCount = 0 then
                    1
                else
                    untypedParameterCount

            // Always ensure one type remains
            // This is a last resort measure though.
            let skip =
                if skip = allTypesFromFunctionType.Length then
                    allTypesFromFunctionType.Length - 1
                else
                    skip

            allTypesFromFunctionType
            |> List.skip skip
            |> List.map (fun t ->
                let formattedType = t.Format symbolUse.DisplayContext

                if t.IsFunctionType then
                    $"({formattedType})"
                else
                    formattedType
            )
            |> String.concat " -> "

let private (|NewPattern|_|) (pat : SynPat) =
    match pat with
    | SynPat.LongIdent (longDotId = SynLongIdent (id = [ newIdent ])) when newIdent.idText = "new" -> Some ()
    | _ -> None

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
            match symbolUse with
            | Mfv mfv -> Some (symbolUse, mfv)
            | _ -> None
        )
        |> Option.defaultWith (fun () -> failwithf $"Could not find symbol for %s{ident.idText}")

    let valueCouldBeMadePrivateToFile =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.And _
        | SynLeadingKeyword.Extern _
        | SynLeadingKeyword.Let _
        | SynLeadingKeyword.LetRec _ -> valueCouldBeMadePrivateToFile env symbol
        | _ -> false

    // Does every parameter has a fully type?
    let untypedParameters =
        match headPat with
        | SynPat.LongIdent (argPats = SynArgPats.Pats (pats = pats)) ->
            pats
            |> List.indexed
            |> List.choose (fun (idx, pat) ->
                let mPat = pat.Range
                let pat = removePatternParens pat

                match pat with
                | SynPat.Const (constant = SynConst.Unit) -> None
                | TypedPatWithoutWildcards _ -> None
                | SynPat.Tuple (elementPats = elements) when allTypedPatterns elements -> None
                | untypedPat ->
                    match Seq.tryItem idx mfv.CurriedParameterGroups with
                    | None ->
                        failwithf
                            $"There is no curried parameter group for this untyped parameter (%i{idx}) in %s{ident.idText}"
                    | Some pg ->
                        let sourceText = env.SourceText.GetContentAt untypedPat.Range

                        if pg.Count = 1 then
                            let singleParameter = pg.[0]
                            let t = singleParameter.Type.Format symbol.DisplayContext

                            match untypedPat with
                            | SynPat.Typed (pat = SynPat.Named (ident = SynIdent (ident = ident)))
                            | SynPat.Named (ident = SynIdent (ident = ident)) ->
                                Some (
                                    MissingParameterType.SingleParameter
                                        {
                                            TypeName = t
                                            Range = mPat
                                            ParameterName = ident.idText
                                            SourceText = ident.idText
                                            InsideConstructor = false
                                        }
                                )
                            | _ ->
                                Some (
                                    MissingParameterType.Pattern
                                        {
                                            TypeName = t
                                            SourceText = sourceText
                                            Range = mPat
                                            Index = idx
                                        }
                                )
                        else
                            match untypedPat with
                            | TuplePatWithUntyped (total, untypedInTuple) when total = pg.Count ->
                                untypedInTuple
                                |> List.map (fun (idx, parameterName, m, isOptional) ->
                                    let t =
                                        // Strip the option type
                                        let fsharpType =
                                            if not isOptional then
                                                pg.[idx].Type
                                            else
                                                pg.[idx].Type.GenericArguments.[0]

                                        fsharpType.Format symbol.DisplayContext

                                    let sourceText = env.SourceText.GetContentAt m

                                    {
                                        TypeName = t
                                        Range = m
                                        ParameterName = parameterName
                                        SourceText = sourceText
                                        InsideConstructor = false
                                    }
                                )
                                |> MissingParameterType.SimpleTupleParameter
                                |> Some
                            | _ ->

                            // This is a tuple which was has some more complex pattern to it.
                            let tupleType =
                                // TODO: maybe wrap in parentheses to be on the safe side?
                                pg
                                |> Seq.map (fun p -> p.Type.Format symbol.DisplayContext)
                                |> String.concat " * "

                            let sourceText = env.SourceText.GetContentAt untypedPat.Range

                            Some (
                                MissingParameterType.Pattern
                                    {
                                        Index = idx
                                        Range = mPat
                                        SourceText = sourceText
                                        TypeName = tupleType
                                    }
                            )
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
        let mkMissingReturnType () =
            let returnTypeText = mkReturnType untypedParameterCount symbol mfv

            Some
                {
                    TypeName = returnTypeText
                    Equals =
                        trivia.EqualsRange
                        |> Option.defaultWith (fun () -> failwithf $"No equals sign in binding %s{ident.idText}")
                }

        match returnInfo with
        | None ->
            match headPat with
            | NewPattern -> None
            | _ -> mkMissingReturnType ()
        | Some (SynBindingReturnInfo (typeName = t)) ->
            if not (hasWildCardInSynType t) then
                None
            else
                mkMissingReturnType ()

    // Are there inferred constraints not in the source.
    let typarDeclsOpt =
        match headPat with
        | SynPat.LongIdent (typarDecls = typarDecls) ->
            typarDecls
            |> Option.bind (fun (SynValTyparDecls (typars = typarDecls)) -> typarDecls)
        | _ -> None

    let missingGenericParameterInfos =
        match headPat with
        // Secondary constructors cannot have generic type parameters
        | NewPattern -> []
        | _ -> findMissingGenericParameterInfos env symbol typarDeclsOpt

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
                Declaration =
                    Declaration.Binding (ident, trivia.LeadingKeyword, trivia.InlineKeyword, missingReturnType)
                Parameters = untypedParameters
                GenericParameters = missingGenericParameterInfos
                ValueCouldBeMadePrivateToFile = valueCouldBeMadePrivateToFile
                CurrentGenericParameters = typarDeclsOpt
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
                    let sourceText = env.SourceText.GetContentAt simplePat.Range

                    Some (
                        MissingParameterType.SingleParameter
                            {
                                TypeName = mfv.FullType.Format symbolUse.DisplayContext
                                Range = ident.idRange
                                ParameterName = ident.idText
                                SourceText = sourceText
                                InsideConstructor = true
                            }
                    )
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
    | SynMemberDefn.Open _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Interface _
    | SynMemberDefn.AbstractSlot _
    | SynMemberDefn.Inherit _ -> []
    | SynMemberDefn.Member (memberDefn = memberDefn) -> processBinding env memberDefn |> Option.toList
    | SynMemberDefn.GetSetMember (memberDefnForGet, memberDefnForSet, range, trivia) ->
        let mkForBinding b =
            b |> Option.bind (processBinding env) |> Option.toList

        [ yield! mkForBinding memberDefnForGet ; yield! mkForBinding memberDefnForSet ]
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
                let valueCouldBeMadePrivateToFile = valueCouldBeMadePrivateToFile env symbolUse
                let parameters = processSimplePats env ctorArgs
                let genericParameters = findMissingGenericParameterInfos env symbolUse typeParams

                if List.isEmpty parameters && List.isEmpty genericParameters then
                    None
                else
                    Some
                        {
                            Declaration = Declaration.ImplicitCtor (List.last typeName, ctorArgs.Range)
                            Parameters = parameters
                            GenericParameters = genericParameters
                            ValueCouldBeMadePrivateToFile = valueCouldBeMadePrivateToFile
                            CurrentGenericParameters = typeParams
                        }
            )
            |> Option.toList
    | SynMemberDefn.ValField (fieldInfo, range) -> failwithf "todo: SynMemberDefn.ValField %A %s" range range.FileName
    | SynMemberDefn.AutoProperty (ident = ident ; typeOpt = typeOpt) ->
        match typeOpt with
        | Some _ -> []
        | None ->

        let lineText = env.SourceText.GetLineString (ident.idRange.EndLine - 1)

        env.CheckFileResults.GetSymbolUseAtLocation (
            ident.idRange.EndLine,
            ident.idRange.EndColumn,
            lineText,
            [ ident.idText ]
        )
        |> Option.bind (fun symbolUse ->
            match symbolUse with
            | Mfv mfv ->
                let t = mkReturnType 0 symbolUse mfv
                let valueCouldBeMadePrivateToFile = valueCouldBeMadePrivateToFile env symbolUse

                Some
                    {
                        Declaration = Declaration.AutoProperty (ident, t)
                        Parameters = []
                        GenericParameters = []
                        ValueCouldBeMadePrivateToFile = valueCouldBeMadePrivateToFile
                        // I think auto properties cannot have generics, not sure though
                        CurrentGenericParameters = None
                    }
            | _ -> None
        )
        |> Option.toList

let rec private processModuleDecl (env : Env) (mdl : SynModuleDecl) =
    match mdl with
    | SynModuleDecl.Let (bindings = bindings) -> List.choose (processBinding env) bindings
    | SynModuleDecl.Types (typeDefns = typeDefns) ->
        [
            for SynTypeDefn (typeRepr = typeRepr ; members = additionalMembers ; typeInfo = typeInfo) in typeDefns do
                let (SynComponentInfo (longId = typeName ; typeParams = typeParams ; accessibility = vis)) =
                    typeInfo

                match vis with
                | Some (SynAccess.Private _) -> ()
                | _ ->

                match typeRepr with
                | SynTypeDefnRepr.Simple _
                | SynTypeDefnRepr.Exception _ -> ()
                | SynTypeDefnRepr.ObjectModel (members = members) ->
                    yield! List.collect (processMember typeName typeParams env) members

                yield! List.collect (processMember typeName typeParams env) additionalMembers
        ]
    | SynModuleDecl.Exception (
        exnDefn = SynExceptionDefn (
            exnRepr = SynExceptionDefnRepr (caseName = SynUnionCase (ident = SynIdent (ident, _))) ; members = members)) ->
        List.collect (processMember [ ident ] None env) members
    | SynModuleDecl.NestedModule (moduleInfo = SynComponentInfo (accessibility = vis) ; decls = decls) ->
        match vis with
        | Some (SynAccess.Private _) -> []
        | _ ->
            let innerEnv = { env with InsideNestedModule = true }
            decls |> List.collect (processModuleDecl innerEnv)
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
                InsideNestedModule = false
            }

        modules |> List.collect (processModuleOrNamespace env)
