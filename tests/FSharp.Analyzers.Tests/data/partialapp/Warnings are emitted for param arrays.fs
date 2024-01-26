module M

    let f a b c = $"here is f with {a + b + c}"

    let test () = System.Console.WriteLine("f = {0}", f 11 22)
