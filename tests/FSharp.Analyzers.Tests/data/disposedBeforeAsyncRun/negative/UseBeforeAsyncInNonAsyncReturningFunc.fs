module M

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

let top () =
    let asyncReturningFunc () =
        let f () =
            use t = new DisposableThing()

            async {
                return "hi"
            }

            23
        
        async {
            return "hi"
        } 
    ()
