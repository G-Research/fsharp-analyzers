module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let asyncReturningFunc () =
    let x = 2 + 3
    // should warn
    use theThing = new DisposableThing()
    async {
        return "hi"
    }
