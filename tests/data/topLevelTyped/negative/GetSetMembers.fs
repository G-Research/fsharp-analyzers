module GS

type F() =
    let mutable disableInMemoryProjectReferences = false
    member __.DisableInMemoryProjectReferences
        with get () : bool = disableInMemoryProjectReferences
        and set (value: bool): unit = disableInMemoryProjectReferences <- value

    member _.PrivateGS
        with private get () = disableInMemoryProjectReferences
        and private set value = disableInMemoryProjectReferences <- value
