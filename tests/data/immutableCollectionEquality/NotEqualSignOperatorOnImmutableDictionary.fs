module Sample

let a = System.Collections.Immutable.ImmutableDictionary.Create<int, string>().Add(1, "kevin")
let b = System.Collections.Immutable.ImmutableDictionary.Create<int, string>().Add(1, "kevin")

a <> b
