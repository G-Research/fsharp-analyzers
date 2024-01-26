module M

open System

let asyncReturningFunc () =
    // should warn
    use t1 = new IO.FileStream("x", IO.FileMode.Open)
    // should warn
    use t2 = new IO.FileStream("x", IO.FileMode.Open)
    async {
        return "hi"
    }
