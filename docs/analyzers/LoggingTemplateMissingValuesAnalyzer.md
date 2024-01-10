---
title: LoggingTemplateMissingValuesAnalyzer
category: analyzers
categoryindex: 1
index: 10
---

# LoggingTemplateMissingValuesAnalyzer

## Problem

As param arrays are loosely typed it's easy to miss an expected templated value or even give too many.

```fsharp
do
    use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
    let logger: ILogger = factory.CreateLogger("Program")
    logger.Log(LogLevel.Information, "first {one} second {two}", 23)
```

## Fix

Provide the correct number of values:

```fsharp
do
    use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
    let logger: ILogger = factory.CreateLogger("Program")
    logger.Log(LogLevel.Information, "first {one} second {two}", 23, 42)
```