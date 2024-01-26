module M

open System

let f1() =
    use disp = new IO.FileStream("", IO.FileMode.Append)
    if true then
        async {
            return ""
        }
    else 
        failwith ""

let f2() =
    use disp = new IO.FileStream("", IO.FileMode.Append)
    if true then
        failwith ""
    else
        async {
            return ""
        } 
