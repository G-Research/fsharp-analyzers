module S

type Coordinate = { X : int; Y: int }

let v =
    Microsoft.FSharp.Core.Operators.string<Coordinate>
        {
            X = 1
            Y = 2
        }
