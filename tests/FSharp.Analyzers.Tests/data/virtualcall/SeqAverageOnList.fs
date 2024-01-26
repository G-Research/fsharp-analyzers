module M

let f () =
    let myList = [ 1.0 .. 10.0 ]
    Seq.average myList // should warn
