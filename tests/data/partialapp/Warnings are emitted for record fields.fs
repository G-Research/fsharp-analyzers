module M

let myFunc x y = x + y

type MyRec =
    {
        Field1: int
        Field2: int -> int
    }

let xxx =
    let r =
        {
            Field1 = 23
            Field2 = myFunc 2 // should warn
        }

    r
