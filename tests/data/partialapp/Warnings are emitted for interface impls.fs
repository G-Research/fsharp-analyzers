module M

type MyClass() =
    let mutable x = 0
    let myFunc x y = x + y
    
    interface System.ICloneable with
        member this.Clone() =
            myFunc 1 // should warn
            let y = myFunc 23 // should warn
            failwith "todo"
