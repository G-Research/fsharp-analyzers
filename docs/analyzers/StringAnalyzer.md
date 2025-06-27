---
title: StringAnalyzer
category: analyzers
categoryindex: 1
index: 3
---

# StringAnalyzer

There are multiple analyzers for various string comparison functions:

- String.EndsWith Analyzer
- String.StartsWith Analyzer
- String.IndexOf Analyzer
- String.LastIndexOf Analyzer

## Problem

The [recommendations for string usage](https://learn.microsoft.com/en-us/dotnet/standard/base-types/best-practices-strings#recommendations-for-string-usage) mention calling the overloads using `StringComparison` to increase performance.

```fsharp
"foo".EndsWith("p")
```

## Fix

Signal your intention explicitly by calling an overload.

```fsharp
open System

"foo".EndsWith("p", StringComparison.Ordinal)
```