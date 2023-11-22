---
title: UnionCaseAnalyzer
category: analyzers
categoryindex: 1
index: 4
---

# UnionCaseAnalyzer

## Problem

A discriminated union case with the same name as a case from `FSharp.Core` is considered harmful.
It may shadow the case from `FSharp.Core` and it is to be avoided.

```fsharp
type MyU =
    | Case1
    | None
    | Case3
```

## Fix

Add [`[<RequireQualifiedAccess>]`](https://fsharp.github.io/fsharp-core-docs/reference/fsharp-core-requirequalifiedaccessattribute.html) on the type:

```fsharp
[<RequireQualifiedAccess>]
type MyU =
    | Case1
    | None
    | Case3
```