module GS

type Foo() =
  let mutable disableInMemoryProjectReferences = false
  member __.DisableInMemoryProjectReferences
    with get () = disableInMemoryProjectReferences
    and set (value) = disableInMemoryProjectReferences <- value
