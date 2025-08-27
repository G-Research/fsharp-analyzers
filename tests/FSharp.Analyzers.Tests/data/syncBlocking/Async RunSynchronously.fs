module AsyncRunSynchronously

open System.Threading.Tasks

let testAsyncRunSynchronously () =
    let computation = async { return 42 }
    let result = Async.RunSynchronously computation
    result

let testAsyncRunSynchronouslyWithTimeout () =
    let computation = async { return "hello" }
    let result = Async.RunSynchronously(computation, 1000)
    result

let testAsyncRunSynchronouslyQualified () =
    let computation = async { return true }
    let result = Microsoft.FSharp.Control.Async.RunSynchronously computation
    result