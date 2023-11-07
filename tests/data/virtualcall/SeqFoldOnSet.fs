module M

let f () =
    let mySet = set [1..10]
    Seq.fold (+) 0 mySet // should warn
