module M

let f () =
    let mySet = set [1..10]
    Seq.rev mySet // should not warn
