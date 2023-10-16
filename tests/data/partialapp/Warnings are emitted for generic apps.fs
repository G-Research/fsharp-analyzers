module M

let myGenFunc<'t> x y z = x + y + z

let _ = myGenFunc<int> 23 42 99 // should not warn
let _ = (myGenFunc<int> 23) 42 // should warn
let _ = (myGenFunc<int> 23 42) // should warn
let _ = myGenFunc<int> 23 // should warn
