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
let ``Type definition of static member`` () =
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
    member this.V (x:int) = 5 - x
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
