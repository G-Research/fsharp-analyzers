module M

let f () =
    let mySet = set [1..10]
    Set.fold (+) 0 mySet // should not warn
