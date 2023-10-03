module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y

    member _.MyMember x =
        myFunc 1 // should warn
        let y = myFunc 23 // should warn
        myFunc 3 44 // should not warn

    member _.MyMember2 x = myFunc 3 // should warn

    member this.X
        with set parameter =
            myFunc 1 // should warn
            let y = myFunc 23 // should warn
            x <- parameter
        and get () = x
