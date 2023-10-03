module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ =
    myFunc
        (23
            |> function
                | 1 -> 23
                | _ -> 42)
        111 // should not warn

let _ =
    myFunc (    // should warn
        23
        |> function
            | 1 -> 23
            | _ -> 42
    )

let _ =
    (23
        |> function
            | 1 -> myFunc   // should not warn because no app
            | _ -> myGenFunc<int> 23)   // should warn
        42

let _ =
    (23
        |> function
            | 1 -> myFunc 23 42
            | _ -> myGenFunc<int> 23 42 101) // should not warn
