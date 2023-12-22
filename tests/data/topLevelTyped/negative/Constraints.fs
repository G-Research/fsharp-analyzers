module C

type Node = interface end

// Subtype of constraint
let enterNode<'n when 'n :> Node> (n: 'n): bool = true
