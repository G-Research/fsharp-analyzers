---
title: TypedInterpolatedStrings Analyzer
category: analyzers
categoryindex: 1
index: 9
---

# TypedInterpolatedStrings Analyzer

Using interpolated strings can catch you off guard when refactoring types.

## Problem

```fsharp
let v = 1 // Changing the type won't affect `s`
let s = $"{v}"
```

## Fix

Add an explicit type format:

```fsharp
let v = 1 // Changing the type will affect `s`
let s = $"%i{v}"
```
