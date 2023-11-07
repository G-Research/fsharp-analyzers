module M

let f () =
    let myList = [3;5]
    let mySet = set [1..10]
    Seq.except myList mySet // should not warn
