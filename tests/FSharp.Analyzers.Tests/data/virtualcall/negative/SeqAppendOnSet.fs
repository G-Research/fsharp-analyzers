module M

let f () =
    let mySet1 = set [1..10]
    let mySet2 = set [7..10]
    Seq.append mySet1 mySet2 // should not warn
