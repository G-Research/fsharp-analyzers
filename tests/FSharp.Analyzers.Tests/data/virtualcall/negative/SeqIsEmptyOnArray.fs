module M

let f () =
    let myArray = [| 1..10 |]
    Seq.isEmpty myArray // should warn
