module NonAsyncMethods

open System
open System.Collections.Generic

let testRegularMethods () =
    let list = List<int>()
    list.Add(42)
    let count = list.Count
    let str = "hello"
    let result = str.ToUpper()
    let parsed = Int32.Parse("123")
    parsed

type MyType() =
    member _.Result = 42
    member _.GetResult() = "hello"
    member _.Wait() = ()

let testCustomTypeMethods () =
    let obj = MyType()
    let r1 = obj.Result
    let r2 = obj.GetResult()
    obj.Wait()
    r1 + r2.Length

let testUnrelatedAsync () =
    let asyncValue = Some 42
    match asyncValue with
    | Some v -> v
    | None -> 0