module GetResultMethods

open System.Threading.Tasks
open System.Runtime.CompilerServices

let testTaskAwaiterGetResult () =
    let t = Task.Run(fun () -> 42)
    let awaiter = t.GetAwaiter()
    let result = awaiter.GetResult()
    result

let testTaskAwaiterGetResultGeneric () =
    let t = Task.Run(fun () -> "hello")
    let awaiter = t.GetAwaiter()
    awaiter.GetResult()

let testValueTaskAwaiterGetResult () =
    let vt = ValueTask.FromResult 42
    let awaiter = vt.GetAwaiter()
    awaiter.GetResult()

let testValueTaskAwaiterGetResultGeneric () =
    let vt = ValueTask<string>("hello")
    let awaiter = vt.GetAwaiter()
    let result = awaiter.GetResult()
    result