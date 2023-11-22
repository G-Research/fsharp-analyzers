# G-Research FSharp Analyzers

A curated set of [Ionide SDK analyzers](https://ionide.io/FSharp.Analyzers.SDK/) for F#.
These are based on real-world production scenarios encountered within G-Research.

## Quickstart

Install the [fsharp-analyzers](https://www.nuget.org/packages/fsharp-analyzers) local dotnet tool.

    dotnet new tool-manifest # if you are setting up this repo
    dotnet tool install --local fsharp-analyzers

Add the [FSharp.Analyzers.Build](https://www.nuget.org/packages/FSharp.Analyzers.Build):

```xml
<PackageReference Include="FSharp.Analyzers.Build" Version="0.2.0">
  <PrivateAssets>all</PrivateAssets>
  <IncludeAssets>build</IncludeAssets>
</PackageReference>
```

And the [G-Research.FSharp.Analyzers](https://www.nuget.org/packages/G-Research.FSharp.Analyzers):

```xml
<PackageReference Include="G-Research.FSharp.Analyzers" Version="0.3.1">
  <PrivateAssets>all</PrivateAssets>
  <IncludeAssets>analyzers</IncludeAssets>
</PackageReference>
```

Include the following MSBuild property:

```xml
<PropertyGroup>
    <FSharpAnalyzersOtherFlags>--analyzers-path &quot;$(PkgG-Research_FSharp_Analyzers)/analyzers/dotnet/fs&quot;</FSharpAnalyzersOtherFlags>
</PropertyGroup>
```

And invoke:

    dotnet msbuild /t:AnalyzeFSharpProject

from the command line.

## Further reading

To learn more about Ionide F# analyzers, you can read the official [Getting started](https://ionide.io/FSharp.Analyzers.SDK/content/Getting%20Started%20Using.html) guide.

<img src="./img/logo.png" alt="G-Research Open Source logo" style="max-width: 300px; margin-top: var(--spacing-600);" />
