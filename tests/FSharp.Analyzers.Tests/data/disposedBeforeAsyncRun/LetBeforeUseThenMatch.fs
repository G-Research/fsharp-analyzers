namespace N

open System

type DisposableThing () =
    interface IDisposable with
        member _.Dispose() = ()

[<RequireQualifiedAccess>]
module DisposableThing =
    let make () =
        new DisposableThing (), 5

module Program =
    let pushFiles () =
        let blah, anInt = DisposableThing.make ()
        use blah = blah
        match anInt with
        | 3 -> failwith "it wasn't 5"
        | _ ->
            async {
                return "hi"
            }
