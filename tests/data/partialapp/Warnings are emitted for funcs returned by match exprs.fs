module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ =
        (match 123 with
         | 1 -> myGenFunc<int> 1 2 // should warn
         | 2 -> myFunc 1) // should warn
            22

let _ =
    (match 123 with
        | 1 -> myGenFunc<int> 1 2 3
        | 2 -> myFunc 1 2) // should not warn
