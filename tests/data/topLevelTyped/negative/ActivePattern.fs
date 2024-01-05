module A

let (|AllAndLast|_|) (xs: 'T list) : ('T list * 'T) option =
    match xs with
    | [] -> None
    | _ ->
      let revd = List.rev xs
      Some(List.rev revd.Tail, revd.Head)
