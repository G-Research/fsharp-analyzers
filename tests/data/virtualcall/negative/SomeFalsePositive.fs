module Meh

let findAllFilesRecursively (folder: string) : string array = Array.empty

let x (folders: string list) = 
    seq {
        yield! (Seq.collect findAllFilesRecursively folders)
    }

(*
let x (folders: string list) = 
    seq {
        yield! (List.collect findAllFilesRecursively folders)
    }
*)
