module C

type Node = interface end

// Subtype of constraint
let enterNode<'n when 'n :> Node> (n: 'n): bool = true

// TyparStaticReq.HeadType
let inline h<^b> (c: ^b) : bool = (obj()).Equals(c)

// TyparStaticReq.None
let g<'f> (a: 'f): unit = ()
