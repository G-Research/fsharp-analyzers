module CC

let merge (first: Map<'a, 'b>) (second: Map<'a, 'b>) : Map<'a,'b> =
    let mutable result = first

    for (KeyValue(key, value)) in second do
      result <- Map.add key value result

    result
