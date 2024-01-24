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

If this is not an issue for your context, you can disable the warning with a comment on top of the `use` statement.
```fsharp
let f () =
    // Note: disposed before returned
    use t = new DisposableThing()
    async {
        return "hi"
    }
```
