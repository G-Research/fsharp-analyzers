(*** hide ***)
#r "../src/FSharp.Analyzers/bin/Release/net8.0/FSharp.Compiler.Service.dll"
#r "../src/FSharp.Analyzers/bin/Release/net8.0/FSharp.Analyzers.SDK.dll"
#r "../src/FSharp.Analyzers/bin/Release/net8.0/G-Research.FSharp.Analyzers.dll"

(**
# G-Research FSharp Analyzers

A curated set of [Ionide SDK analyzers](https://ionide.io/FSharp.Analyzers.SDK/) for F#.
These are based on real-world production scenarios encountered within G-Research.

## Quickstart

Install the [fsharp-analyzers](https://www.nuget.org/packages/fsharp-analyzers) local dotnet tool.
*)

(*** hide ***)
FSharp.Analyzers.SDK.Utils.currentFSharpAnalyzersSDKVersion
|> fun v ->
    $"""
<pre class="fssnip highlighted"><code>dotnet new tool-manifest # if you are setting up this repo
dotnet tool install --local fsharp-analyzers --version %i{v.Major}.%i{v.Minor}.%i{v.Revision}
</code></pre>
"""
(*** include-it-raw ***)

(**
Add the [FSharp.Analyzers.Build](https://www.nuget.org/packages/FSharp.Analyzers.Build):

```shell
<PackageReference Include="FSharp.Analyzers.Build" Version="0.2.0">
  <PrivateAssets>all</PrivateAssets>
  <IncludeAssets>build</IncludeAssets>
</PackageReference>
```

And the [G-Research.FSharp.Analyzers](https://www.nuget.org/packages/G-Research.FSharp.Analyzers):
*)

(*** hide ***)
typeof<GR.FSharp.Analyzers.TypeAnnotateStringFunctionAnalyzer.StringApplicationResult>.Assembly
    .GetName()
    .Version
|> fun v ->
    $"""
<pre class="fssnip highlighted"><code>&lt;PackageReference Include=&quot;G-Research.FSharp.Analyzers&quot; Version=&quot;%i{v.Major}.%i{v.Minor}.%i{v.Revision}&quot;&gt;
  &lt;PrivateAssets&gt;all&lt;/PrivateAssets&gt;
  &lt;IncludeAssets&gt;analyzers&lt;/IncludeAssets&gt;
&lt;/PackageReference&gt;
</code></pre>
    """
(*** include-it-raw ***)

(**
Include the following MSBuild property:

```xml
<PropertyGroup>
    <FSharpAnalyzersOtherFlags>--analyzers-path &quot;$(PkgG-Research_FSharp_Analyzers)/analyzers/dotnet/fs&quot;</FSharpAnalyzersOtherFlags>
</PropertyGroup>
```

And invoke:

```shell
dotnet msbuild /t:AnalyzeFSharpProject
```

from the command line.

## Further reading

To learn more about Ionide F# analyzers, you can read the official [Getting started](https://ionide.io/FSharp.Analyzers.SDK/content/Getting%20Started%20Using.html) guide.

<img src="./img/logo.png" alt="G-Research Open Source logo" style="max-width: 300px; margin-top: var(--spacing-600);" />
*)
