---
title: DisposedBeforeAsyncRunAnalyzer
category: analyzers
categoryindex: 1
index: 11
---

# DisposedBeforeAsyncRunAnalyzer

## Problem

A `use` statement in an `async` or `task` returning function can be problematic as the value is disposed before the returned workflow is run.

```fsharp
let f () =
    use t = new DisposableThing()
    async {
        return "hi"
    }
```

## Fix

If applicable to your context, move the `use` statement into the `async`.

```fsharp
let f () =
    async {
        use t = new DisposableThing()
        return "hi"
    }
```

If the `use` before the `async` is really your intent, you can disable the warning with a line comment on top of the `use` statement containing `disposed before returned workflow runs`.
```fsharp
let f () =
    // Note: disposed before returned workflow runs
    use t = new DisposableThing()
    async {
        return "hi"
    }
```
