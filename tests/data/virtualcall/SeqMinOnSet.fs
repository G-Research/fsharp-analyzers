module M

let f () =
    let mySet = set [1..10]
    mySet |> Seq.min // should warn
