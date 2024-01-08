module C

type Foo<'a,'b when 'a : equality>(a: 'a, b: 'b) =
    let _ = a.Equals(b)

type private Bar(a) =
    do ignore<int> a

open System.Threading.Tasks

type ConstantVal<'a>(value: Task<'a>) =
    new(x:int, value: Task<'a>) = ConstantVal(value)
