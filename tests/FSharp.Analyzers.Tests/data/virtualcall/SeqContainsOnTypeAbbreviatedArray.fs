module M

[<RequireQualifiedAccess>]
type SyntaxNode =
    | SynPat of int
    | SynType of int

type SyntaxVisitorPath = SyntaxNode array

let f (path: SyntaxVisitorPath) =
    Seq.length path // should warn
