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

type MissingParameterType =
    {
        /// Return type name formatted using proper DisplayContext.
        TypeName : string
        /// Source text of the parameter pattern
        SourceText : string
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
        /// It might still be used outside the project.
        ValueIsUsedOutsideTheFileInTheProject : bool
    }

type FSharpGenericParameter with

    member private gp.HasEqualityConstraint =
        gp.Constraints |> Seq.exists (fun c -> c.IsEqualityConstraint)

    member private gp.SubtypesOfTypeConstraint (dp : FSharpDisplayContext) =
        gp.Constraints
        |> Seq.choose (fun c ->
            if not c.IsCoercesToConstraint then
                None
            else
                let t = c.CoercesToTarget.Format (dp)
                Some $"%s{gp.Name} :> %s{t}"
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

let private symbolHasUsesOutsideFile (env : Env) (symbolUse : FSharpSymbolUse) =
    match symbolUse with
    // Active patterns can't be detected it seems.
    | Mfv mfv when mfv.IsActivePattern -> true
    | _ ->

    env.CheckProjectResults.GetUsesOfSymbol symbolUse.Symbol
    |> Array.exists (fun su -> su.FileName <> symbolUse.FileName)

let private findMissingGenericParameterInfos
    (env : Env)
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
            let typars =
                decls
                |> List.map (fun (SynTyparDecl (typar = SynTypar (ident = ident))) ->
                    ident.idText, UntypedGenericParameterInfo.Empty
                )
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
                | SynTypeConstraint.WhereTyparIsEquatable (SynTypar (ident = ident), range) ->
                    let typarInfo =
                        Map.tryFind ident.idText map
                        |> Option.defaultValue UntypedGenericParameterInfo.Empty

                    Map.add
                        ident.idText
                        { typarInfo with
                            IsEqualityConstraint = true
                        }
                        map
                | SynTypeConstraint.WhereTyparDefaultsToType (typar, typeName, range) ->
                    failwithf "todo: SynTypeConstraint.WhereTyparDefaultsToType %A %s" range range.FileName
                | SynTypeConstraint.WhereTyparSubtypeOfType (SynTypar (ident = ident), typeName, range) ->
                    let typarInfo =
                        Map.tryFind ident.idText map
                        |> Option.defaultValue UntypedGenericParameterInfo.Empty

                    let typeText = env.SourceText.GetContentAt typeName.Range

                    Map.add
                        ident.idText
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
                    match Map.tryFind gp.Name untypedConstraints with
                    | None ->
                        let subtypeConstraints = gp.SubtypesOfTypeConstraint (symbolUse.DisplayContext)

                        [
                            if gp.HasEqualityConstraint then
                                yield $"%s{gp.Name} : equality"
                            yield! subtypeConstraints
                        ]
                    | Some untypedConstraint ->
                        let subtypeConstraints = gp.SubtypesOfTypeConstraint (symbolUse.DisplayContext)

                        [
                            if untypedConstraint.IsEqualityConstraint <> gp.HasEqualityConstraint then
                                yield $"%s{gp.Name} : equality"
                            if untypedConstraint.SubtypesOfType.Length <> subtypeConstraints.Length then
                                yield! subtypeConstraints
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
            match symbolUse with
            | Mfv mfv -> Some (symbolUse, mfv)
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
                        failwithf
                            $"There is no curried parameter group for this untyped parameter (%i{idx}) in %s{ident.idText}"
                    | Some pg ->
                        if pg.Count = 1 then
                            let singleParameter = pg.[0]

                            let parameterName =
                                match untypedPat with
                                | SynPat.Named (ident = SynIdent (ident = ident)) -> Some ident.idText
                                | _ -> None

                            let sourceText = env.SourceText.GetContentAt (untypedPat.Range)

                            Some
                                {
                                    Index = idx
                                    TypeName = singleParameter.Type.Format symbol.DisplayContext
                                    Range = untypedPat.Range
                                    ParameterName = parameterName
                                    AddParentheses = Option.isNone parameterName
                                    SourceText = sourceText
                                }
                        else
                            let tupleType =
                                // TODO: maybe wrap in parentheses to be on the safe side?
                                pg
                                |> Seq.map (fun p -> p.Type.Format symbol.DisplayContext)
                                |> String.concat " * "

                            let sourceText = env.SourceText.GetContentAt (untypedPat.Range)

                            Some
                                {
                                    Index = idx
                                    ParameterName = None
                                    TypeName = tupleType
                                    Range = untypedPat.Range
                                    AddParentheses = true
                                    SourceText = sourceText
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
                        mfv.ReturnParameter.Type.Format symbol.DisplayContext
                    else
                        let skip =
                            // In case of a value member like  `member this.V = 4`
                            // The return type will be: `OwnerType -> unit -> value`
                            if
                                mfv.IsMember
                                && allTypesFromFunctionType.Length = 3
                                && untypedParameters.Length = 0
                            then
                                2
                            elif mfv.IsMember then
                                1 + untypedParameterCount
                            else
                                untypedParameterCount

                        allTypesFromFunctionType
                        |> List.skip skip
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
                        |> Option.defaultWith (fun () -> failwithf $"No equals sign in binding %s{ident.idText}")
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

            findMissingGenericParameterInfos env symbol typeParams
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
                    let sourceText = env.SourceText.GetContentAt (simplePat.Range)

                    Some
                        {
                            TypeName = mfv.FullType.Format symbolUse.DisplayContext
                            Range = ident.idRange
                            Index = idx
                            ParameterName = Some ident.idText
                            AddParentheses = false
                            SourceText = sourceText
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
    | SynMemberDefn.Open _
    | SynMemberDefn.ImplicitInherit _
    | SynMemberDefn.Interface _
    | SynMemberDefn.AbstractSlot _ -> []
    | SynMemberDefn.Member (memberDefn = memberDefn) -> processBinding env memberDefn |> Option.toList
    | SynMemberDefn.GetSetMember (memberDefnForGet, memberDefnForSet, range, trivia) ->
        failwithf "todo: SynMemberDefn.GetSetMember %A %s" range range.FileName
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
                let genericParameters = findMissingGenericParameterInfos env symbolUse typeParams

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
    | SynMemberDefn.Inherit (baseType, asIdent, range) ->
        failwithf "todo: SynMemberDefn.Inherit %A %s" range range.FileName
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
                let t = mfv.FullType.Format symbolUse.DisplayContext
                let usedOutsideFile = symbolHasUsesOutsideFile env symbolUse

                Some
                    {
                        Declaration = Declaration.AutoProperty (ident, t)
                        Parameters = []
                        GenericParameters = []
                        ValueIsUsedOutsideTheFileInTheProject = usedOutsideFile
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
        | _ -> decls |> List.collect (processModuleDecl env)
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
