module M

let f (g: _ -> unit) : unit = ignore<int -> unit> g; ()

let h (i: int, j: _) : int = ignore<string> j; 0

let k : _ -> int = printfn "meh"; fun (a: int) -> a + 1
