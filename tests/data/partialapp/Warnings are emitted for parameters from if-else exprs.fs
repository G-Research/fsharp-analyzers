module M

let myFunc x y = x + y

let _ = myFunc (if true then 1 else 0) // should warn
let _ = myFunc (if true then 1 else 0) (if true then 1 else 0) // should not warn
