module I

open System

ignore $"Hey %i{42}"
// One format specifier per string: FCS 43.13.x reports false type errors when an interpolated string
// has several specifiers and one of them is not a bare %s, %c, %d or %i.
// Combine these again once a release carries the fix.
// Issue: https://github.com/dotnet/fsharp/issues/20752
// Fix: https://github.com/dotnet/fsharp/pull/20775
ignore $"%f{4.}"
ignore $"a %A{obj()}"
ignore $"b %O{TimeSpan.Zero}"
ignore $"%0.17f{43.2}"
ignore $"%b{false}"
