module M

let f () =
    let mySeq1 = Seq.ofList [1..10]
    let mySeq2 = Seq.ofList [100..1000]
    Seq.append mySeq1 mySeq2 // should not warn
