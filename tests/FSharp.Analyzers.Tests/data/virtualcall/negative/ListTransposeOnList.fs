module M

let f () =
    let myList = [ [1]; [2]; [3] ]
    List.transpose myList // should not warn
