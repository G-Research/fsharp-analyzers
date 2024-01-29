module M

let f () =
    let rs = ResizeArray()
    Seq.tryFind (fun _ -> true) rs // should not warn
