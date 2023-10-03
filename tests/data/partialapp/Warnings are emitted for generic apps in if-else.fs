module M

let myFunc x y = x + y
let myGenFunc<'t> x y z = x + y + z

let _ = (if true then myGenFunc<int> 23 42 else myFunc 23) 88 // should warn
