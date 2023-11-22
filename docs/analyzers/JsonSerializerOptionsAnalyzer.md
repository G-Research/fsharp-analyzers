---
title: JsonSerializerOptionsAnalyzer
category: analyzers
categoryindex: 1
index: 1
---

# JsonSerializerOptionsAnalyzer

## Problem

The [Performance Improvements in .NET 8](https://devblogs.microsoft.com/dotnet/performance-improvements-in-net-8/#json) blog post mentions `JsonSerializerOptions` from `System.Text.Json` is deceptively expensive to create, so calls to JsonSerializer.{De}Serialize should use a cached instance rather than newing one up each time.

```fsharp
open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Deserialize<string>(json, JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsync<string>(jsonStream, JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsyncEnumerable<string>(jsonStream, JsonSerializerOptions ())
    ()
```

## Fix

Try and cache the `JsonSerializerOptions` instance:

```fsharp
open System.Text.Json

let f (json : string) (jsonStream : System.IO.Stream) =
    let options = JsonSerializerOptions ()
    let _ = JsonSerializer.Deserialize<string>(json, options)
    let _ = JsonSerializer.DeserializeAsync<string>(jsonStream, options)
    let _ = JsonSerializer.DeserializeAsyncEnumerable<string>(jsonStream, options)
    ()
```