module Sample

let a = System.Collections.Immutable.ImmutableHashSet.Create<float>().Add(1.)
let b = System.Collections.Immutable.ImmutableHashSet.Create<float>().Add(1.)

a.Equals(b)
