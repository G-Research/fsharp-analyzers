---
title: SyncBlocking Analyzer
category: analyzers
categoryindex: 1
index: 
---

# SyncBlocking Analyzer

## Problem

Calls to blocking methods and properties like `Task.Wait` and `Task.Result`, or `Async.RunSynchronously`, consume a thread in the thread pool for as long as they are running.
The .NET runtime tries to cope with this by spinning up new operating system threads if it detects thread pool starvation, but [there is a maximum number of threads](https://learn.microsoft.com/en-us/dotnet/standard/threading/the-managed-thread-pool#maximum-number-of-thread-pool-threads), and attempting to exceed this number will cause the application to deadlock once all threads are blocked waiting for a synchronous call to complete.

```fsharp
let doTheThing (input : Task<int>) : int =
    // This line blocks
    let input = input.Result
    input + 1
```

## Fix

The correct fix depends on context.

## In a library function

Usually the correct answer is to propagate the asynchronous nature of the workflow up into the function signature:

```fsharp
let doTheThing (input : Task<int>) : Task<int> =
    task {
        // Correct: await the asynchronous workflow
        let! input = input
        return input + 1
    }
```

## In the main method
F# [does not support async main methods](https://github.com/dotnet/fsharp/issues/11631#issuecomment-855052325), so a blocking call will always be necessary in the entry point.

```fsharp
[<EntryPoint>]
let main argv =
    doTheThing ()
    // ANALYZER: synchronous blocking call allowed in entry point
    |> Async.RunSynchronously
```

## In tests

Many standard test runners support asynchronous tests out of the box.

Avoid synchronous blocking in tests.
Test runners may give you a much more restrictive thread pool than you are used to; this can result in heavy slowdowns or deadlocks in tests even when you wouldn't expect to see them in prod.

```fsharp
open NUnit.Framework

// NUnit supports Task and Async out of the box, for example
[<Test>]
let myAsyncTest () = task {
    let! result = computeMyResult ()
    result |> shouldEqual 8
}
```

## If you already have a proof that the access is safe

You might have some proof that a `Task` has already completed at the time you want to access it.
In that case, just suppress the analyzer at the point of access.

```fsharp
if myTask.IsCompletedSuccessfully then
    // SAFETY: synchronous blocking call allowed because task has completed
    myTask.Result
else
    ...
```
