module M

let f () =
    let mySet = set [1..10]
    Set.contains 2 mySet // should not warn
