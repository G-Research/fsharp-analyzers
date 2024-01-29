module M

open System

let f1() =
    use disp = new IO.FileStream("", IO.FileMode.Append)
    
    try
        task {
            return ""
        }
    with
    | _ -> failwith ""

let f2() =
    use disp = new IO.FileStream("", IO.FileMode.Append)
    
    try
        task {
            return ""
        }
    finally
        ()
