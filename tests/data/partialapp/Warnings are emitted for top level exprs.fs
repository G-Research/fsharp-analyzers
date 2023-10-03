module M

let myFunc x y = x + y

myFunc 123 // shoud warn

module SubMod =

    myFunc 123 // shoud warn
