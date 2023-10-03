module M

let myFunc x y = x + y

let myFuncWithMatch x =
    match x with
    | 1 ->
        let a = myFunc 23 // should warn
        true
    | _ ->
        let a = myFunc 23 42 // should not warn
        false
