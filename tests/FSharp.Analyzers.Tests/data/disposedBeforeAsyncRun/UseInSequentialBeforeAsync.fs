module M

open System

let asyncReturningFunc () =
    id 3; id 4
    use t = new IO.FileStream("", IO.FileMode.Append)
    async {
        return "hi"
    }
