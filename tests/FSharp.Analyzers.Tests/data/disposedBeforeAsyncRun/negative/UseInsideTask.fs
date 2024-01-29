module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let taskReturningFunc () =
    task {
        use t = new DisposableThing()
        return "hi"
    }
