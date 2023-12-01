---
title: LoggingArgFuncNotFullyAppliedAnalyzer
category: analyzers
categoryindex: 1
index: 8
---

# LoggingArgFuncNotFullyAppliedAnalyzer

## Problem

As param arrays are loosely typed it's easy to miss a partially applied function in the display params of ILogger methods.

```fsharp
let f x y = x + y

do
    use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
    let logger: ILogger = factory.CreateLogger("Program")
    logger.Log(LogLevel.Information, "f returned: {0}", f 11)
```

## Fix

Always apply all the arguments:

```fsharp
let f x y = x + y

do
    use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
    let logger: ILogger = factory.CreateLogger("Program")
    logger.Log(LogLevel.Information, "f returned: {0}", f 11 22)
```