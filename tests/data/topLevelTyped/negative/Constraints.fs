module C

type Node = interface end

// Subtype of constraint
let enterNode<'n when 'n :> Node> (n: 'n): bool = true

// TyparStaticReq.HeadType
let inline h<^b> (c: ^b) : bool = (obj()).Equals(c)

// TyparStaticReq.None
let g<'f> (a: 'f): unit = ()

// Comparison constraint
let merge<'a, 'b when 'a :comparison> (first: Map<'a, 'b>) (second: Map<'a, 'b>) : Map<'a,'b> =
    let mutable result = first

    for (KeyValue(key, value)) in second do
      result <- Map.add key value result

    result
