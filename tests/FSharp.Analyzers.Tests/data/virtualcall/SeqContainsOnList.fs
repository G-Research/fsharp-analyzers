module M

let f () =
    let myList = [1..10]
    Seq.contains 2 myList // should warn
