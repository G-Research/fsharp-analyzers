module M

open System
open System.IO

let asyncReturningFunc () =
    do
        use t1 = new System.IO.FileStream("", IO.FileMode.Open)
        use t2 = new System.IO.FileStream("", IO.FileMode.Open)
        ()
    async {
        return "hi"
    }