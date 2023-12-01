---
title: ImmutableCollectionEquality Analyzer
category: analyzers
categoryindex: 1
index: 7
---

# ImmutableCollectionEquality Analyzer

## Problem

[ImmutableDictionary<TKey,TValue>](https://learn.microsoft.com/en-us/dotnet/api/system.collections.immutable.immutabledictionary-2?view=net-8.0) and [ImmutableHashSet<T>](https://learn.microsoft.com/en-us/dotnet/api/system.collections.immutable.immutablehashset-1?view=net-8.0) do not have an implementation for structural equality.  
Calling `.Equals` on them or using `=` can be misleading as the reference equality will be checked.  
See [source code for Immutable collections](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Collections.Immutable/src/System/Collections/Immutable/ImmutableDictionary_2.cs)

```fsharp
open System.Collections.Immutable

let a = ImmutableDictionary.Create<int, string>().Add(1, "Dave")
let b = ImmutableDictionary.Create<int, string>().Add(1, "Dave")

// Analyzer will trigger
a = b
```

## Fix

It is more 

```fsharp
open System.Collections.Immutable

let a = ImmutableDictionary.Create<int, string>().Add(1, "Dave")
let b = ImmutableDictionary.Create<int, string>().Add(1, "Dave")

// This makes it more clear that only reference equality is checked.
Object.ReferenceEquals(a, b)
```
