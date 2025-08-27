module TaskResultProperty

open System.Threading.Tasks

let testTaskResult () =
    let t = Task.Run(fun () -> 42)
    let result = t.Result
    result

let testTaskOfTResult () =
    let t : Task<string> = Task.Run(fun () -> "hello")
    t.Result

let testChainedResult () =
    let result = Task.Run(fun () -> 42).Result
    result

let testValueTaskResult () =
    let vt = ValueTask<int>(42)
    let result = vt.Result
    result