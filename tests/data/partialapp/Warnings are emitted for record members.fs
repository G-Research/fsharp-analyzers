module M

let myFunc x y = x + y

type MyRec =
    {
        Field1: int
        Field2: int -> int
    }

    static member MyRecMember x = myFunc 23 x // should not warn
    static member MyRecMember2 x = myFunc x // should warn
