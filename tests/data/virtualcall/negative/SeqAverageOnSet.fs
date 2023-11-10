module M

let f () =
    let mySet = set [1.0 .. 10.0]
    Seq.average mySet // should not warn
