module WithSuppressionComment

open System.Threading.Tasks

let testAsyncRunSynchronouslyWithComment () =
    let computation = async { return 42 }
    // synchronous blocking call allowed
    let result = Async.RunSynchronously computation
    result

let testTaskWaitWithComment () =
    let t = Task.Run(fun () -> 42)
    t
    // ANALYZER: synchronous blocking call allowed for testing
    |> fun x -> x.Wait()
    // ANALYZER: synchronous blocking call allowed for testing
    t.Result

let testTaskResultWithCommentAbove () =
    let t = Task.Run(fun () -> "hello")
    // Synchronous Blocking Call Allowed (case insensitive)
    t.Result

let testWithBlockComment () =
    let t = Task.Run(fun () -> 42)
    (* synchronous blocking call allowed *)
    t.Wait()

let testGetResultWithComment () =
    let t = Task.Run(fun () -> 42)
    let awaiter = t.GetAwaiter()
    // synchronous blocking call allowed in main
    awaiter.GetResult()