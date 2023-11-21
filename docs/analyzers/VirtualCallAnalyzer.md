---
title: VirtualCall Analyzer
category: analyzers
categoryindex: 1
index: 5
---

# VirtualCall Analyzer

## Problem

Certain `Seq` operations should be avoided when the input collection type is known and has a specific module counter-part.
This can avoid a virtual call at runtime and the specific collection implementation is more performant.
Example: Use `Set.fold` rather than `Seq.fold` when the argument is a set.

```fsharp
let f () =
    let mySet = set [1..10]
    Seq.fold (+) 0 mySet // should warn
```

## Fix

Use the `Set` counterpart

```fsharp
let f () =
    let mySet = set [1..10]
    Set.fold (+) 0 mySet
```