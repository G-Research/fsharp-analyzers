module M

let f () =
    let mySet = set [1..10]
    Seq.max mySet // should warn
