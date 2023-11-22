---
title: TypeAnnotateStringFunctionAnalyzer
category: analyzers
categoryindex: 1
index: 6
---

# TypeAnnotateStringFunction

## Problem

Using the `string` function can catch you off guard when refactoring types.

```fsharp
let v = 1 // Changing this value won't affect `s`
let s = string v
```

## Fix

Add an explicit type annotation:

```fsharp
let v = 1 // Changing this will affect `s`
let s = string<int> v
```