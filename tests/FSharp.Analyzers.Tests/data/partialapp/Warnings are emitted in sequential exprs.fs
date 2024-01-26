module M

let myFunc x y = x + y

let partapp1 =
    myFunc 1 // should warn
    let y = myFunc 23 // should warn
    myFunc 3 44 // should not warn
