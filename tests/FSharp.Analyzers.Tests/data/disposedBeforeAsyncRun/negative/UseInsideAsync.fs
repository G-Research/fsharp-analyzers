module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let asyncReturningFunc () =
    async {
        use t = new DisposableThing()
        return "hi"
    }
