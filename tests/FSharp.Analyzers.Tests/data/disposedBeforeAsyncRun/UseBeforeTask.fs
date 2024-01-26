module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let taskReturningFunc () =
    use t = new DisposableThing()
    task {
        return "hi"
    }
