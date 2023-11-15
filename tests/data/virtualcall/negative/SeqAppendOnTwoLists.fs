module M

let f () =
    let myList1 = [1..10]
    let myList2 = [100..1000]
    Seq.append myList1 myList2 // should warn
