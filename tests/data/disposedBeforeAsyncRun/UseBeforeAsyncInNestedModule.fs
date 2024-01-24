module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

module M2 =
    let asyncReturningFunc () =
        // should warn
        use t = new DisposableThing()
        async {
            return "hi"
        }
