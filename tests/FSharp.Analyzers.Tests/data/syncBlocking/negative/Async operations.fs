module AsyncOperations

open System.Threading.Tasks

let testAsyncComputation () = async {
    let! result = async { return 42 }
    return result
}

let testTaskComputation () = task {
    let! result = Task.Run(fun () -> 42)
    return result
}

let testAsyncStartWithContinuation () =
    let computation = async { return 42 }
    Async.StartWithContinuations(
        computation,
        (fun result -> printfn $"Result: %d{result}"),
        (fun exn -> printfn $"Error: %O{exn}"),
        (fun cancel -> printfn "Cancelled")
    )

let testAsyncStartAsTask () =
    let computation = async { return 42 }
    let task = Async.StartAsTask computation
    task

let testTaskRun () =
    let t = Task.Run(fun () -> 42)
    t

let testValueTask () =
    let vt = ValueTask<int>(42)
    vt