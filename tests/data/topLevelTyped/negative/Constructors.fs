module C

type Foo<'a,'b when 'a : equality>(a: 'a, b: 'b) =
    let _ = a.Equals(b)

type private Bar(a) =
    do ignore<int> a