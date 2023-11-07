module M

let f () =
    let mySet = set [1..10]
    Seq.contains 2 mySet // should warn
