module M

let f () =
    let myList = [1.0 .. 10.0]
    Seq.averageBy id myList // should warn
