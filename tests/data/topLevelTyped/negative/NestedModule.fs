module NM

module private Nested =
    let a = ""
    let b c = c - 1

    type D(e) =
        do
            printfn "%s" e

        member x.F = 'f'
        member val G = 23.
        member x.H i j = i + j + 1
