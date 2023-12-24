module GR.FSharp.Analyzers.Tests.TopLevelTypedAnalyzerTests

open System.Collections
open System.IO
open NUnit.Framework
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK.Testing
open GR.FSharp.Analyzers
open GR.FSharp.Analyzers.Tests.Common
open TopLevelTypeAnalysis

let mutable projectOptions : FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUp>]
let Setup () =
    task {
        let! options = mkOptionsFromProject "net7.0" []
        projectOptions <- options
    }

type TestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "topLevelTyped" |]

[<TestCaseSource(typeof<TestCases>)>]
let TopLevelTypedAnalyzerTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> TopLevelTypedAnalyzer.topLevelTypedAnalyzer

        do! assertExpected fileName messages
    }

type NegativeTestCases() =

    interface IEnumerable with
        member _.GetEnumerator () : IEnumerator =
            constructTestCaseEnumerator [| "topLevelTyped" ; "negative" |]

[<TestCaseSource(typeof<NegativeTestCases>)>]
let NegativeTests (fileName : string) =
    task {
        let fileName = Path.Combine (dataFolder, fileName)

        let! messages =
            File.ReadAllText fileName
            |> getContext projectOptions
            |> TopLevelTypedAnalyzer.topLevelTypedAnalyzer

        Assert.That (messages, Is.Empty)
    }

[<Test>]
let ``Type definition of static value member`` () =
    async {
        let source =
            """
module M

type T =
    static member V = 5
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo.Declaration with
        | Declaration.Binding (returnType = Some returnType) -> Assert.That (returnType.TypeName, Is.EqualTo "int")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Type definition of member`` () =
    async {
        let source =
            """
module M

type T =
    member this.F (x:int) = 5 - x
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo.Declaration with
        | Declaration.Binding (returnType = Some returnType) -> Assert.That (returnType.TypeName, Is.EqualTo "int")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Type definition of member value`` () =
    async {
        let source =
            """
module M

type T =
    member this.V = 4
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo.Declaration with
        | Declaration.Binding (returnType = Some returnType) -> Assert.That (returnType.TypeName, Is.EqualTo "int")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Type definition of static function member`` () =
    async {
        let source =
            """
module M

type MultilineFormatterType =
    | CharacterWidth
    | NumberOfItems

    static member ToConfigString(cfg: MultilineFormatterType)  =
        match cfg with
        | MultilineFormatterType.CharacterWidth -> "character_width"
        | MultilineFormatterType.NumberOfItems -> "number_of_items"
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo.Declaration with
        | Declaration.Binding (returnType = Some returnType) -> Assert.That (returnType.TypeName, Is.EqualTo "string")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Missing parameter type in tuple`` () =
    async {
        let source =
            """
module M

[<Sealed>]
type CodeFormatter =
    static member ParseAsync(isSignature, source) : int =
        ignore<string> source
        if isSignature then 0 else 1
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo.Parameters with
        | [ MissingParameterType.SimpleTupleParameter (items = [ isSignatureInfo ; sourceInfo ]) ] ->
            Assert.That (isSignatureInfo.TypeName, Is.EqualTo "bool")
            Assert.That (sourceInfo.TypeName, Is.EqualTo "string")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``All typed tuple pattern in member`` () =
    async {
        let source =
            """
namespace Foo

[<Sealed>]
type CodeFormatter =
    static member TransformAST (ast: string, isSignature: bool) : int = 0
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [] -> Assert.Pass ()
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Type of auto property`` () =
    async {
        let source =
            """
namespace Foo

type Bar(content: string, idx: int) =
    member val Content = content
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults
            |> List.head

        match missingInfo with
        | {
              Declaration = Declaration.AutoProperty (name, typeName)
          } ->
            Assert.That (name.idText, Is.EqualTo "Content")
            Assert.That (typeName, Is.EqualTo "string")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }




// TODO: bindings in nested module can not be private when the module is private?
// See Context.fs
