module M

[<Sealed>]
type T =
    static member Transform (source: string) : int = source.Length
    member x.Foo () : string = "meh"
    member inline x.BindReturn(v: int, [<InlineIfLambda>] mapping: int -> int) : int = 0
