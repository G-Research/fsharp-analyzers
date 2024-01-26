module M

open System

let f () =
    
    let asyncReturningFunc () =
        // should warn
        use t1 = new IO.FileStream("x", IO.FileMode.Open)
        // should warn
        use t2 = new IO.FileStream("x", IO.FileMode.Open)
        async {
            // should not warn
            use t3 = new IO.FileStream("x", IO.FileMode.Open)
            return 23
    }

    asyncReturningFunc ()
