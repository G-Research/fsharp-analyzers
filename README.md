# G-Research Fsharp Analyzers

![GitHub Workflow Status (event)](https://img.shields.io/github/actions/workflow/status/G-Research/fsharp-analyzers/release.yml?branch=main&label=CI&style=flat-square)
[![Nuget (with prereleases)](https://img.shields.io/nuget/vpre/G-Research.FSharp.Analyzers?style=flat-square)](https://www.nuget.org/packages/G-Research.FSharp.Analyzers/absoluteLatest)

A curated set of [Ionide SDK analyzers](https://ionide.io/FSharp.Analyzers.SDK/) for F#.

## Usage

To run any analyzer you need to find a way to download the NuGet package locally.
At the time of writing there is no standard way to do this, one way this can be done is by using the [<PackageDownload>](https://learn.microsoft.com/en-us/nuget/consume-packages/packagedownload-functionality)

```xml
<Project Sdk="Microsoft.Build.NoTargets/1.0.80"> <!-- This is not a project we want to build. -->

  <PropertyGroup>
    <IsPackable>false</IsPackable>
    <RestorePackagesPath>./.analyzerpackages/</RestorePackagesPath> <!-- Changes the global packages folder-->
    <!-- <MSBuildProjectExtensionsPath>$(RestorePackagesPath)obj/</MSBuildProjectExtensionsPath> --> <!-- It's still PackageReference, so project intermediates are still created. -->
    <TargetFramework>net6.0</TargetFramework> <!-- This is not super relevant, as long as your SDK version supports it. -->
    <DisableImplicitNuGetFallbackFolder>true</DisableImplicitNuGetFallbackFolder> <!-- If a package is resolved to a fallback folder, it may not be downloaded.-->
    <AutomaticallyUseReferenceAssemblyPackages>false</AutomaticallyUseReferenceAssemblyPackages> <!-- We don't want to build this project, so we do not need the reference assemblies for the framework we chose.-->
  </PropertyGroup>

  <ItemGroup>
    <PackageDownload Include="G-Research.FSharp.Analyzers" Version="[0.1.4]" />
  </ItemGroup>

</Project>
```

Running `dotnet restore` will download this locally to `./.analyzerpackages`.

Then you need to install the [fsharp-analyzers tool](https://www.nuget.org/packages/fsharp-analyzers).

Next you can run `dotnet fsharp-analyzers --project YourProject.fsproj --analyzers-path ./.analyzerpackages/g-research.fsharp.analyzers/0.1.4/lib/net6.0`

Please checkout the documentation over at [FSharp.Analyzers.SDK](https://ionide.io/FSharp.Analyzers.SDK/).

## Contributing

We welcome new contributors! We'll happily receive PRs for bug fixes
or small changes. If you're contemplating something larger please get
in touch first by opening a GitHub Issue describing the problem and
how you propose to solve it.

### Scaffold new analyzer

Run

    dotnet fsi build.fsx -- -p NewAnalyzer

to scaffold a new analyzer.

## License

Copyright &copy; 2023 G-Research

Licensed under the Apache License, Version 2.0 (the "License"); you may not use these files except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

