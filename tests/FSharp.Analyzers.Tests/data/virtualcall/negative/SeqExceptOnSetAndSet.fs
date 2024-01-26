module M

let f () =
    let mySet1 = set [3;5]
    let mySet2 = set [1..10]
    Seq.except mySet1 mySet2 // should warn
