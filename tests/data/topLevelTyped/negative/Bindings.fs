module B

// Value
let v : int = 0

// Function
let f (a:int) (b:int) : int = a + b

// Constraints
let areEqual<'a, 'b when 'a : equality> (a: 'a) (b: 'b) : bool = a.Equals(b)
