module M

let f () =
    let myList1 = [100..1000]
    let myList2 = [100..1000]
    let myList3 = [100..1000]
    Seq.map3 (fun x y z -> x + y + z) myList1 myList2 myList3 // should warn
