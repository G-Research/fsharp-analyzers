module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let taskReturningFunc () =
    // Note: disposed before returned workflow runs
    use t = new DisposableThing()
    task {
        return "hi"
    }
