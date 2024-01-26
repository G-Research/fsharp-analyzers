module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y

    let partapp1 =
        myFunc 1 // should warn
        let y = myFunc 23 // should warn
        myFunc 3 44 // should not warn

    let partapp2 = myFunc 4 // should warn
    let partapp3 = (+) 4 55 // should not warn
    let partapp4 = (+) 4 // should warn
    let partapp5: (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
