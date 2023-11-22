---
title: PartialAppAnalyzer
category: analyzers
categoryindex: 1
index: 2
---

# PartialAppAnalyzer

## Problem

In some code bases partial application is to be avoided in order to improve the debug experience.

```fsharp
let myFunc x y = x + y

do
    let f = myFunc 1 // should warn
    myFunc 2 // should warn
    ()
```

## Fix

Always apply all the arguments:

```fsharp
let myFunc x y = x + y

do
    let v = myFunc 1 2
    ()
```