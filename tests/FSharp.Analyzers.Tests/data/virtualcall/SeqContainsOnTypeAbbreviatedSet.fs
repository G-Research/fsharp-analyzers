module M

[<RequireQualifiedAccess>]
type SyntaxNode =
    | SynPat of int
    | SynType of int

type SyntaxVisitorPath = Set<SyntaxNode>

let f (path: SyntaxVisitorPath) =
    Seq.length path // should warn
