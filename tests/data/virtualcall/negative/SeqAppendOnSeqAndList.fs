module M

let f () =
    let mySeq = Seq.ofList [1..10]
    let myList = [100..1000]
    Seq.append mySeq myList // should not warn
