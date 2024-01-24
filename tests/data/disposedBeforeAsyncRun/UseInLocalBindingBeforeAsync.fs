module M

open System
open System.IO

let asyncReturningFunc () =
    let localFunc () =
        // should warn
        use t = new System.IO.FileStream("", IO.FileMode.Open)
        t |> ignore
        ()
    localFunc ()
    async {
        return "hi"
    }
