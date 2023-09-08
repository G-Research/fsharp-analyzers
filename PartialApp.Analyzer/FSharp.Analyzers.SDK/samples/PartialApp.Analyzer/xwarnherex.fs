namespace MyNamespace

module MyMod =

    let myFunc x y = x + y
    let myFuncWithMatch x =
        match x with
        | 1 ->
            let a = myFunc 23       // should warn
            true
        | _ ->
            let a = myFunc 23 42    // should not warn
            false
    
    type MyU =
        | Case1
        | Case2
        static member MyUnionMember x = myFunc 23 x // should not warn
        static member MyUnionMember2 x = myFunc x // should warn
        
    type MyRec = {
        Field1: int
        Field2: int -> int
    }
    with
        static member MyRecMember x = myFunc 23 x // should not warn
        static member MyRecMember2 x = myFunc x // should warn

    let myGenFunc<'t> x y z = x + y + z
    let _ = myGenFunc<int> 23 42 99 // should not warn
    let _ = (myGenFunc<int> 23) 42  // should warn
    let _ = (myGenFunc<int> 23 42)  // should warn
    let _ = myGenFunc<int> 23       // should warn
    
    let _ = (if true then myGenFunc<int> 23 42 else myFunc 23) 88   // should warn
    
    let _ = (match 123 with | 1 -> myGenFunc<int> 1 2 | 2 -> myFunc 1) 22   // should warn
    let _ = (match 123 with | 1 -> myGenFunc<int> 1 2 3 | 2 -> myFunc 1 2)  // should not warn
    
    let _ = myFunc (if true then 1 else 0)  // should warn
    let _ = myFunc (if true then 1 else 0) (if true then 1 else 0) // should not warn
    
    let _ = myFunc (23 |> function | 1 -> 23 | _ -> 42) 111 // should not warn
    let _ = myFunc (23 |> function | 1 -> 23 | _ -> 42)     // should warn
    
    let _ = (23 |> function | 1 -> myFunc | _ -> myGenFunc<int> 23) 42              // should warn
    let _ = (23 |> function | 1 -> myFunc 23 42 | _ -> myGenFunc<int> 23 42 101)    // should not warn
    
    let xxx =
        let r = {
            Field1 = 23
            Field2 = myFunc 2   // should warn
        }
        r
    
    let partapp1 =
        myFunc 1            // should warn
        let y = myFunc 23   // should warn
        myFunc 3 44  // should not warn
    let partapp2 = myFunc 4     // should warn
    let partapp3 = (+) 4 55     // should not warn
    let partapp4 = (+) 4        // should warn
    let partapp5 : (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
    myFunc 123  // shoud warn  
    
    module SubMod =
        
        let myFunc x y = x + y
        let partapp1 = myFunc 3 44  // should not warn
        let partapp2 = myFunc 4     // should warn
        let partapp3 = (+) 4 55     // should not warn
        let partapp4 = (+) 4        // should warn
        let partapp5 : (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
        myFunc 123  // shoud warn
        
    type MyClass() =
        let mutable x = 0
        let myFunc x y = x + y
        let partapp1 =
            myFunc 1            // should warn
            let y = myFunc 23   // should warn
            myFunc 3 44         // should not warn
        let partapp2 = myFunc 4     // should warn
        let partapp3 = (+) 4 55     // should not warn
        let partapp4 = (+) 4        // should warn
        let partapp5 : (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn
        member _.MyMember x =
            myFunc 1      // should warn
            let y = myFunc 23   // should warn
            myFunc 3 44   // should not warn
        member _.MyMember2 x = myFunc 3     // should warn
        member this.X
            with set parameter =
                myFunc 1            // should warn
                let y = myFunc 23   // should warn
                x <- parameter
            and get() = x
            
        interface System.ICloneable with
            member this.Clone() =
                myFunc 1            // should warn
                let y = myFunc 23   // should warn
                failwith "todo"
        
        type MyNestedClass() =
            let myFunc x y = x + y
            let partapp1 = myFunc 3 44  // should not warn
            let partapp2 = myFunc 4     // should warn
            let partapp3 = (+) 4 55     // should not warn
            let partapp4 = (+) 4        // should warn
            let partapp5 : (int seq -> int seq) = Seq.map (fun x -> x + 1) // should warn

    module M =
         let myFunc x y = x + y
         do
             let f = myFunc 1    // should warn
             myFunc 1            // should warn
             ()