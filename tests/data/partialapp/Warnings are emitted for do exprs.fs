module M
    module S =
        let myFunc x y = x + y

        do
            let f = myFunc 1 // should warn
            myFunc 1 // should warn
            ()
