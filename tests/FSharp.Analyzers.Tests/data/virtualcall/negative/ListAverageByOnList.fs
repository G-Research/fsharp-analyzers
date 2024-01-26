module M

let f () =
    let myList = [1.0 .. 10.0]
    List.averageBy id myList // should not warn
