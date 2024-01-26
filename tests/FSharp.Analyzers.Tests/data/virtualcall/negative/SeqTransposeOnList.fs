module M

let f () =
    let myList = [ [1]; [2]; [3] ]
    Seq.transpose myList // should not warn
