module GR.FSharp.Analyzers.Tests.TopLevelTypedAnalyzerTests

open System.Collections
open System.IO
open FSharp.Analyzers.SDK
open FSharp.Compiler.Text
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

[<Test>]
let ``Types of override members`` () =
    async {
        let source =
            """
namespace Foo

open System

[<CustomEquality; CustomComparison>]
type F =
    | V of int
    override x.Equals(other: obj) = false
    override x.GetHashCode() = 0
    interface IComparable with
        member x.CompareTo other = -1
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ {
                Declaration = Declaration.Binding (returnType = Some equalsReturnType)
            }
            {
                Declaration = Declaration.Binding (returnType = Some getHashCodeReturnType)
            } ] ->
            Assert.That (equalsReturnType.TypeName, Is.EqualTo "bool")
            Assert.That (getHashCodeReturnType.TypeName, Is.EqualTo "int")
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``A property cannot have explicit type parameters`` () =
    async {
        let source =
            """
namespace Foo

type Queue<'T>(data: 'T array list, length: int) =
    member x.Head =
        if length > 0 then
            (List.head data).[0]
        else
            raise (System.Exception("Queue is empty"))
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ { GenericParameters = [] } ] -> Assert.Pass ()
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``A binding in a nested module`` () =
    async {
        let source =
            """
namespace Foo

module WriterModel =
    let init = 4
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ { GenericParameters = [] } ] -> Assert.Pass ()
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Option parameter type is missing`` () =
    async {
        let source =
            """
namespace Foo

type Bar() =
    member x.SomeMethod(maxWidth: int, ?startColumn) : int = ignore<int option> startColumn ; 7
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ {
                Parameters = [ MissingParameterType.SimpleTupleParameter (items = [ { TypeName = "int" } ]) ]
            } ] -> Assert.Pass ()
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Missing parameter type is missing should include parentheses in reported range`` () =
    async {
        let source =
            """
module X

let f (g: _): int = ignore<string> g; 0
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ {
                Parameters = [ MissingParameterType.SingleParameter ({ ParameterName = "g" ; Range = m }) ]
            } ] ->
            Assert.That ((m.StartLine, m.StartColumn), Is.EqualTo (4, 6))
            Assert.That ((m.EndLine, m.EndColumn), Is.EqualTo (4, 12))
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

[<Test>]
let ``Missing generic parameter type is only reported once`` () =
    async {
        let source =
            """
module X

open System
open System.Threading.Tasks
    
let mapWithAdditionalDependencies (mapping: 'a -> 'b) (value: Task<'a>) : Task<'b> =
    let o : 'a = failwith "meh"
    let aa = value.Result
    ignore<bool> (aa = o)
    let b = mapping aa
    let bb = b :> IDisposable
    let bbb = b :> ICloneable
    failwith<Task<'b>> "meh"
    """

        let ctx = getContext projectOptions source

        let missingInfo =
            findMissingTypeInformation
                ctx.SourceText
                ctx.ParseFileResults.ParseTree
                ctx.CheckFileResults
                ctx.CheckProjectResults

        match missingInfo with
        | [ {
                GenericParameters = [ {
                                          Name = "'a"
                                          MissingConstraints = [ "'a : equality" ]
                                      }
                                      {
                                          Name = "'b"
                                          MissingConstraints = [ "'b :> IDisposable" ; "'b :> ICloneable" ]
                                      } ]
            } ] -> Assert.Pass ()
        | d -> Assert.Fail $"Unexpected declaration %A{d}"
    }

// [<Test>]
let foobar () =
    async {
        let checker = FSharpChecker.Create ()

        let args =
            File.ReadAllLines @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Fantomas.Core.rsp"

        let options =
            checker.GetProjectOptionsFromCommandLineArgs ("Fantomas.Core.fsproj", args)

        let file = @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\Context.fs"

        do! checker.NotifyFileChanged (file, options) // workaround for https://github.com/dotnet/fsharp/issues/15960
        let! projectCheckResults = checker.ParseAndCheckProject options
        let allSymbolUses = projectCheckResults.GetAllUsesOfAllSymbols ()

        let sourceText = SourceText.ofString (File.ReadAllText file)

        let! parseResult, checkResults = checker.ParseAndCheckFileInProject (file, 1, sourceText, options)

        match checkResults with
        | FSharpCheckFileAnswer.Aborted -> ()
        | FSharpCheckFileAnswer.Succeeded checkResults ->
            let missingInfo =
                findMissingTypeInformation sourceText parseResult.ParseTree checkResults projectCheckResults

            ignore missingInfo
    }

(*
Up next:


*)

// TODO: bindings in nested module can not be private when the module is private?
// See Context.fs