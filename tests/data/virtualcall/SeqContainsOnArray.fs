module M

let f () =
    let myArray = [| 1..10 |]
    Seq.contains 2 myArray // should warn
