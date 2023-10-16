module M

let myFunc x y = x + y

type MyU =
    | Case1
    | Case2

    static member MyUnionMember x = myFunc 23 x // should not warn
    static member MyUnionMember2 x = myFunc x // should warn
