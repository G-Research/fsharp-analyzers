module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let asyncReturningFunc () =
    // Note: disposed before returned async runs
    use t = new DisposableThing()
    async {
        return "hi"
    }
