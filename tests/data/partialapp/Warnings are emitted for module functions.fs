module M

let partapp5: (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
