module M

let findAllFilesRecursively (folder: string) : string array = Array.empty

let x (folders: string list) = 
    seq {
        yield! (Seq.collect findAllFilesRecursively folders)
    }
