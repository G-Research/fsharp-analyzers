module M

let f () =
    let mySeq1 = Seq.ofList [1..10]
    let mySeq2 = Seq.ofList [1..10]
    let myList = [100..1000]
    Seq.map3 (fun x y z -> x + y + z) mySeq1 myList mySeq2 // should not warn
