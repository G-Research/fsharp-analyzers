#!/usr/bin/env -S dotnet fsi --

#r "nuget: NuGet.Protocol, 7.9.0"

#load "format.fsx"

// Opens the daily "Update FSharp.Analyzers.SDK" pull request.
// See .github/workflows/bump-analyzers-sdk.yml.
// Upstream releases usually carry an FCS bump along with them, which is more than Dependabot can
// reason about. Nothing here merges anything: a human reads the release notes and presses the button.
//
//   dotnet fsi bump-sdk.fsx             the real thing, needs GH_TOKEN
//   dotnet fsi bump-sdk.fsx --dry-run   edit the files and render the body, write to neither git nor GitHub

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Text.RegularExpressions
open System.Threading
open NuGet.Common
open NuGet.Frameworks
open NuGet.Protocol
open NuGet.Protocol.Core.Types
open NuGet.Versioning

let (</>) a b = Path.Combine (a, b)

let repositoryRoot = __SOURCE_DIRECTORY__
let packagesProps = repositoryRoot </> "Directory.Packages.props"
let toolManifest = repositoryRoot </> ".config" </> "dotnet-tools.json"
let changelogFile = repositoryRoot </> "CHANGELOG.md"

let analyzersProject =
    repositoryRoot </> "src" </> "FSharp.Analyzers" </> "FSharp.Analyzers.fsproj"

let upstream = "ionide/FSharp.Analyzers.SDK"
let repository = "G-Research/fsharp-analyzers"
let baseBranch = "main"
let sdkPackage = "FSharp.Analyzers.SDK"
let testingPackage = "FSharp.Analyzers.SDK.Testing"
let toolPackage = "fsharp-analyzers"

/// Everything after the version bump itself talks to git or to GitHub. A dry run stops at the
/// working tree, which is what you want when you are trying the script out locally.
let dryRun = Array.contains "--dry-run" fsi.CommandLineArgs

// ---------------------------------------------------------------------------------------------
// running other programs
// ---------------------------------------------------------------------------------------------

/// ArgumentList rather than a command string, so a pull request title never has to be quoted.
let private start (fileName : string) (arguments : string list) (redirect : bool) =
    printfn $"""> %s{fileName} %s{String.concat " " arguments}"""

    let startInfo =
        ProcessStartInfo (fileName, WorkingDirectory = repositoryRoot, RedirectStandardOutput = redirect)

    List.iter startInfo.ArgumentList.Add arguments
    use proc = Process.Start startInfo
    let output = if redirect then proc.StandardOutput.ReadToEnd () else ""
    proc.WaitForExit ()

    if proc.ExitCode <> 0 then
        failwith $"%s{fileName} exited with %i{proc.ExitCode}"

    output

let private exec (fileName : string) (arguments : string list) =
    start fileName arguments false |> ignore

let private capture (fileName : string) (arguments : string list) = (start fileName arguments true).Trim()

// The three programs this script drives.
let private dotnet = exec "dotnet"
let private git = exec "git"
let private gh = exec "gh"
let private ghOutput = capture "gh"

// ---------------------------------------------------------------------------------------------
// nuget
// ---------------------------------------------------------------------------------------------

// NuGet.Protocol rather than the flat container over raw HTTP: it knows what a version is, and it
// picks a dependency group the way restore would.
let private feed =
    Repository.Factory.GetCoreV3 "https://api.nuget.org/v3/index.json"

let private packages = feed.GetResource<FindPackageByIdResource>()
let private cache = new SourceCacheContext ()

let private versionsOf (packageId : string) : NuGetVersion list =
    packages.GetAllVersionsAsync (packageId, cache, NullLogger.Instance, CancellationToken.None)
    |> Async.AwaitTask
    |> Async.RunSynchronously
    |> List.ofSeq

/// The framework the analyzers are built for, which decides which dependency group applies. Asked
/// of MSBuild rather than read out of the fsproj, because the property does not have to be written
/// there to be what the project builds as. Lazy, so a morning with nothing to do runs no MSBuild.
let private targetFramework =
    lazy
        (capture "dotnet" [ "msbuild" ; analyzersProject ; "-getProperty:TargetFramework" ]
         |> NuGetFramework.Parse)

/// FSharp.Core and FSharp.Compiler.Service are whatever the SDK itself depends on: it loads the
/// analyzer into its own process, so a different version is an error waiting to happen. None when
/// NuGet does not have that version of the SDK at all.
let private dependencyPins (version : NuGetVersion) : (NuGetVersion * NuGetVersion) option =
    let info =
        packages.GetDependencyInfoAsync (sdkPackage, version, cache, NullLogger.Instance, CancellationToken.None)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    if isNull (box info) then
        None
    else

    let framework = targetFramework.Value

    match NuGetFrameworkUtility.GetNearest (info.DependencyGroups, framework) with
    | null ->
        failwith
            $"%s{sdkPackage} %s{version.ToNormalizedString ()} has no dependency group for %s{framework.GetShortFolderName ()}."
    | group ->

    let versionOf (packageId : string) =
        group.Packages
        |> Seq.tryFind (fun dependency -> String.Equals (dependency.Id, packageId, StringComparison.OrdinalIgnoreCase))
        |> function
            | Some dependency -> dependency.VersionRange.MinVersion
            | None -> failwith $"%s{sdkPackage} %s{version.ToNormalizedString ()} does not depend on %s{packageId}."

    Some (versionOf "FSharp.Core", versionOf "FSharp.Compiler.Service")

// ---------------------------------------------------------------------------------------------
// the files the bot edits
// ---------------------------------------------------------------------------------------------

/// Deliberately a string replacement rather than an XML round-trip: the diff has to be the four
/// attribute values that changed and nothing else.
let private packageVersionRegex (packageId : string) =
    Regex
        $"""(?<prefix><PackageVersion\s+Include="%s{Regex.Escape packageId}"\s+Version=")(?<version>[^"]*)(?<suffix>")"""

/// Every entry in that file is an exact-version range, "[0.39.0]" rather than "0.39.0", and that is
/// the point: the analyzer is loaded into the SDK's own process, so resolving a higher FSharp.Core
/// or FCS than it was built against is a load failure. Anything but an exact pin is a mistake worth
/// stopping for, and the brackets go no further than these two functions.
let private readPackageVersion (content : string) (packageId : string) : NuGetVersion =
    let matches = (packageVersionRegex packageId).Matches content

    if matches.Count <> 1 then
        failwith
            $"Expected exactly one PackageVersion for %s{packageId} in Directory.Packages.props, found %i{matches.Count}."

    let text = matches.[0].Groups.["version"].Value
    let range = VersionRange.Parse text

    if
        not (
            range.IsMinInclusive
            && range.IsMaxInclusive
            && range.MinVersion = range.MaxVersion
        )
    then
        failwith $"%s{packageId} is %s{text} in Directory.Packages.props, which is not an exact pin."

    range.MinVersion

let private replacePackageVersion (packageId : string) (version : NuGetVersion) (content : string) =
    let regex = packageVersionRegex packageId

    if regex.Matches(content).Count <> 1 then
        failwith $"Expected exactly one PackageVersion for %s{packageId} in Directory.Packages.props."

    let pinned = VersionRange(version, true, version, true).ToLegacyShortString()
    regex.Replace (content, $"${{prefix}}%s{pinned}${{suffix}}", 1)

/// The fsharp-analyzers entry of the tool manifest, leaving fantomas and fsdocs-tool alone.
let private toolVersionRegex =
    Regex """(?<prefix>"fsharp-analyzers"\s*:\s*\{[^}]*?"version"\s*:\s*")(?<version>[^"]*)(?<suffix>")"""

let private readToolVersion (content : string) : NuGetVersion =
    let matches = toolVersionRegex.Matches content

    if matches.Count <> 1 then
        failwith $"Expected exactly one fsharp-analyzers version in .config/dotnet-tools.json, found %i{matches.Count}."

    NuGetVersion.Parse matches.[0].Groups.["version"].Value

let private replaceToolVersion (version : NuGetVersion) (content : string) =
    toolVersionRegex.Replace (content, $"${{prefix}}%O{version}${{suffix}}", 1)

// ---------------------------------------------------------------------------------------------
// is there a bump to make
// ---------------------------------------------------------------------------------------------

type Plan =
    {
        Version : string
        Branch : string
        Title : string
    }

/// The number of the open pull request for a branch, if there is one.
let private openPullRequestFor (branch : string) : int option =
    ghOutput
        [
            "pr"
            "list"
            "--head"
            branch
            "--state"
            "open"
            "--json"
            "number"
            "--jq"
            ".[0].number // empty"
        ]
    |> fun output -> if output = "" then None else Some (int output)

/// Not a Result: nothing to do is not a failure. A release can be tagged minutes before its
/// packages are indexed, and a red X in the Actions tab every time Ionide ships would teach
/// everyone to ignore that tab. Tomorrow's run picks it up.
type Bump =
    | Pending of Plan
    | NothingToDo of reason : string

/// The newest upstream release, when it is newer than what is checked in and both its packages are
/// on NuGet.
let private pendingBump () : Bump =
    let newest =
        ghOutput [ "api" ; $"repos/%s{upstream}/releases/latest" ; "--jq" ; ".tag_name" ]
        |> fun tag -> NuGetVersion.Parse (tag.TrimStart 'v')

    let current = readPackageVersion (File.ReadAllText packagesProps) sdkPackage

    let version = newest.ToNormalizedString ()

    if newest <= current then
        NothingToDo $"%s{sdkPackage} is at %O{current}, the newest release is %s{version}."
    else

    // One release, three packages, indexed separately. The tool counts as much as the two
    // libraries do, because CI restores it from the manifest this bump rewrites.
    let missing =
        [ sdkPackage ; testingPackage ; toolPackage ]
        |> List.tryFind (fun packageId -> not (List.contains newest (versionsOf packageId)))

    match missing with
    | Some packageId -> NothingToDo $"%s{packageId} %s{version} is released on GitHub but not on NuGet yet."
    | None ->

    let plan =
        {
            Version = version
            Branch = $"bump/analyzers-sdk-%s{version}"
            Title = $"Update %s{sdkPackage} to %s{version}"
        }

    // One pull request per release, and once it is open the branch belongs to whoever is working
    // on it. A maintainer adapting to an upstream API break commits onto it, and a run that
    // started over would throw that away. Closing the pull request asks for a fresh one.
    match openPullRequestFor plan.Branch with
    | Some number -> NothingToDo $"#%i{number} is already open for %s{version}."
    | None -> Pending plan

// ---------------------------------------------------------------------------------------------
// the version bump
// ---------------------------------------------------------------------------------------------

/// A version this run changes. The list of them drives the edits and describes them, so the log
/// and the table in the pull request body cannot say something other than what was written.
type Change =
    {
        Package : string
        From : string
        To : string
    }

/// The pins that were checked in against what the SDK version they were checked in for asks for.
/// When these disagree the bump corrects two lines that look unrelated to it, and the reviewer
/// should hear why before they go looking. Takes the props as checked in, so it has to be called
/// before they are rewritten.
let private driftWarning (props : string) =
    let previousSdk = readPackageVersion props sdkPackage

    match dependencyPins previousSdk with
    | None -> None
    | Some (expectedCore, expectedService) ->

    let previousCore = readPackageVersion props "FSharp.Core"
    let previousService = readPackageVersion props "FSharp.Compiler.Service"

    [
        if previousCore <> expectedCore then
            $"`FSharp.Core` was pinned to `%O{previousCore}` while %s{sdkPackage} %O{previousSdk} asks for `%O{expectedCore}`."
        if previousService <> expectedService then
            $"`FSharp.Compiler.Service` was pinned to `%O{previousService}` while %s{sdkPackage} %O{previousSdk} asks for `%O{expectedService}`."
    ]
    |> function
        | [] -> None
        | lines -> Some (String.concat " " lines)

/// Rewrites Directory.Packages.props and .config/dotnet-tools.json, and answers with what it wrote
/// and with the drift, both of which the pull request body reports.
let private bumpVersions (plan : Plan) : Change list * string option =
    let newCore, newService =
        match dependencyPins (NuGetVersion.Parse plan.Version) with
        | Some pins -> pins
        | None -> failwith $"NuGet has no %s{sdkPackage} %s{plan.Version}."

    let version = NuGetVersion.Parse plan.Version

    let pins =
        [
            sdkPackage, version
            testingPackage, version
            "FSharp.Core", newCore
            "FSharp.Compiler.Service", newService
        ]

    let props = File.ReadAllText packagesProps
    let manifest = File.ReadAllText toolManifest

    let changes =
        [
            for package, target in pins do
                {
                    Package = package
                    From = string<NuGetVersion>(readPackageVersion props package)
                    To = string<NuGetVersion> target
                }
            {
                Package = toolPackage
                From = string<NuGetVersion>(readToolVersion manifest)
                To = plan.Version
            }
        ]

    (props, pins)
    ||> List.fold (fun content (package, target) -> replacePackageVersion package target content)
    |> fun content -> File.WriteAllText (packagesProps, content)

    File.WriteAllText (toolManifest, replaceToolVersion version manifest)

    for change in changes do
        printfn $"%s{change.Package} %s{change.From} -> %s{change.To}"

    let drift = driftWarning props
    drift |> Option.iter (fun drift -> printfn $"Drift: %s{drift}")

    changes, drift

// ---------------------------------------------------------------------------------------------
// the changelog
// ---------------------------------------------------------------------------------------------

let private releaseHeaderRegex = Regex @"^##\s+\[?(?<name>[^\]\s]+)\]?"

let private isSectionHeader (line : string) =
    line.StartsWith ("## ", StringComparison.Ordinal)

/// One past the last line of the section starting at `start`.
let private sectionEnd (lines : ResizeArray<string>) (start : int) =
    let mutable index = start + 1

    while index < lines.Count && not (isSectionHeader lines.[index]) do
        index <- index + 1

    index

/// Every SDK bump this repository has shipped was a minor, whatever else rode along with it.
let private nextReleaseVersion (released : string) =
    let parts = released.Split '.'
    $"%s{parts.[0]}.%i{int parts.[1] + 1}.0"

let private appendToChangedSection (lines : ResizeArray<string>) (sectionStart : int) (entry : string) =
    let stop = sectionEnd lines sectionStart

    let subsection (title : string) =
        seq { sectionStart + 1 .. stop - 1 }
        |> Seq.tryFind (fun index -> lines.[index].Trim() = title)

    /// One past the last line of the subsection, the next "###" or the end of the section.
    let subsectionEnd (start : int) =
        let mutable index = start + 1

        while index < stop && not (lines.[index].StartsWith("###", StringComparison.Ordinal)) do
            index <- index + 1

        index

    match subsection "### Changed" with
    | Some changed ->
        let mutable insertAt = subsectionEnd changed

        while insertAt > changed + 1 && String.IsNullOrWhiteSpace lines.[insertAt - 1] do
            insertAt <- insertAt - 1

        lines.Insert (insertAt, entry)

    | None ->

    // Keep a Changelog orders its subsections, and "Added" is the only one that comes before
    // "Changed", so a fresh "### Changed" goes after that one when the section has it.
    match subsection "### Added" with
    | None -> lines.InsertRange (sectionStart + 1, [ "" ; "### Changed" ; "" ; entry ])
    | Some added ->

    let insertAt = subsectionEnd added

    let separator =
        if String.IsNullOrWhiteSpace lines.[insertAt - 1] then
            []
        else
            [ "" ]

    lines.InsertRange (insertAt, separator @ [ "### Changed" ; "" ; entry ; "" ])

/// Rewrites CHANGELOG.md and answers with the version, the date and the lines of the section the
/// entry landed in, which is what the pull request body has to show the reviewer.
let private writeChangelog (plan : Plan) (pullRequest : int) =
    let lines = ResizeArray (File.ReadAllLines changelogFile)

    let headers =
        seq { 0 .. lines.Count - 1 }
        |> Seq.filter (fun index -> isSectionHeader lines.[index])
        |> List.ofSeq

    let nameOf index =
        releaseHeaderRegex.Match(lines.[index]).Groups.["name"].Value

    let firstHeader =
        match headers with
        | [] -> failwith "CHANGELOG.md has no '## ' section."
        | first :: _ -> first

    let unreleased =
        String.Equals (nameOf firstHeader, "Unreleased", StringComparison.OrdinalIgnoreCase)

    let released =
        headers
        |> List.map nameOf
        |> List.tryFind (fun name -> Regex.IsMatch (name, @"^\d+\.\d+\.\d+"))
        |> Option.defaultWith (fun () -> failwith "CHANGELOG.md has no released version to count from.")

    let release = nextReleaseVersion released
    let date = DateTime.UtcNow.ToString ("yyyy-MM-dd", CultureInfo.InvariantCulture)
    let header = $"## [%s{release}] - %s{date}"

    let entry =
        $"- Update %s{sdkPackage} to `%s{plan.Version}`. [#%i{pullRequest}](https://github.com/%s{repository}/pull/%i{pullRequest})"

    // The maintainers promote an Unreleased section rather than leaving it behind, so the entries
    // sitting in it are released along with this bump. The pull request body says so out loud.
    if unreleased then
        lines.[firstHeader] <- header
        appendToChangedSection lines firstHeader entry
    else
        lines.InsertRange (firstHeader, [ header ; "" ; "### Changed" ; "" ; entry ; "" ])

    File.WriteAllLines (changelogFile, lines)
    printfn $"CHANGELOG.md: %s{release} - %s{date}"

    let section =
        seq { firstHeader + 1 .. sectionEnd lines firstHeader - 1 }
        |> Seq.map (fun index -> lines.[index])
        |> String.concat "\n"

    release, date, section.Trim ()

// ---------------------------------------------------------------------------------------------
// the pull request body
// ---------------------------------------------------------------------------------------------

let private renderBody
    (plan : Plan)
    (changes : Change list)
    (drift : string option)
    (release : string)
    (date : string)
    (section : string)
    =
    let table =
        changes
        |> List.map (fun change -> $"| `%s{change.Package}` | `%s{change.From}` | `%s{change.To}` |")
        |> String.concat "\n"

    let warning =
        match drift with
        | None -> ""
        | Some drift ->
            let correction =
                "This pull request corrects that, so those lines change for a reason that has nothing to do with the version in the title."

            $"\n> [!IMPORTANT]\n> %s{drift} %s{correction}\n"

    $"""Opened by [`bump-analyzers-sdk.yml`](https://github.com/%s{repository}/blob/%s{baseBranch}/.github/workflows/bump-analyzers-sdk.yml). It is a first attempt, not a verdict: read the upstream notes and watch CI before merging.

Upstream release notes: <https://github.com/%s{upstream}/releases/tag/v%s{plan.Version}>

| Package | From | To |
| --- | --- | --- |
%s{table}

`FSharp.Core` and `FSharp.Compiler.Service` are not chosen here, they are read from the `<dependencies>` of the new SDK nuspec. The last row is the tool manifest entry in `.config/dotnet-tools.json`, the rest are `Directory.Packages.props`.
%s{warning}
## CHANGELOG.md, %s{release} - %s{date}

%s{section}

CI does not start by itself for a pull request opened with `GITHUB_TOKEN`, so the workflow dispatches `ci.yml` on this branch instead.
"""

// ---------------------------------------------------------------------------------------------
// git and gh
// ---------------------------------------------------------------------------------------------

let private commit (message : string) (paths : string list) =
    git ([ "add" ; "--" ] @ paths)
    git [ "commit" ; "--message" ; message ]

/// Force, because a run that failed after the push leaves the branch behind and the next run for
/// the same version has to be able to start over. It cannot land on a human's commits: an open
/// pull request for this version is what pendingBump stops on.
let private push (plan : Plan) =
    git [ "push" ; "--force" ; "origin" ; $"HEAD:refs/heads/%s{plan.Branch}" ]

/// Puts the version bump on its own branch and opens the pull request for it. A dry run answers
/// with zero, which is the number the changelog entry then links to.
let private openPullRequest (plan : Plan) : int =
    if dryRun then
        0
    else

    // Leave a laptop's git identity alone. On a runner there is none to leave alone.
    if Environment.GetEnvironmentVariable "GITHUB_ACTIONS" = "true" then
        git [ "config" ; "user.name" ; "github-actions[bot]" ]

        git
            [
                "config"
                "user.email"
                "41898282+github-actions[bot]@users.noreply.github.com"
            ]

    git [ "switch" ; "--force-create" ; plan.Branch ]
    commit plan.Title [ packagesProps ; toolManifest ]
    push plan

    // gh refuses a label that does not exist, and this one is the bot's own.
    gh
        [
            "label"
            "create"
            "automated"
            "--color"
            "ededed"
            "--description"
            "Opened by a scheduled workflow"
            "--force"
        ]

    let url =
        ghOutput
            [
                "pr"
                "create"
                "--base"
                baseBranch
                "--head"
                plan.Branch
                "--title"
                plan.Title
                "--body"
                "Writing the details, the next commit fills this in."
                "--label"
                "dependencies"
                "--label"
                "automated"
            ]

    printfn $"Opened %s{url}"
    int (Array.last (url.Split '/'))

/// A new version means a new branch, so yesterday's pull request is strictly superseded. One open
/// bot pull request at a time keeps the review queue honest.
let private closeSuperseded (pullRequest : int) =
    ghOutput
        [
            "pr"
            "list"
            "--state"
            "open"
            "--label"
            "automated"
            "--json"
            "number,headRefName"
            "--jq"
            ".[] | select(.headRefName | startswith(\"bump/analyzers-sdk-\")) | .number"
        ]
    |> fun output -> output.Split ('\n', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun number -> number.Trim ())
    |> Array.filter (fun number -> number <> string<int> pullRequest)
    |> Array.iter (fun number ->
        gh [ "pr" ; "comment" ; number ; "--body" ; $"Superseded by #%i{pullRequest}." ]
        gh [ "pr" ; "close" ; number ; "--delete-branch" ]
    )

/// The other half of openPullRequest: the second commit, the body that could not be written before
/// the number existed, and the CI run that GitHub will not start on its own. A dry run prints the
/// body and leaves it there.
let private finishPullRequest (plan : Plan) (pullRequest : int) (body : string) =
    if dryRun then
        printfn $"\n%s{body}"
    else

    let file = Path.GetTempFileName ()
    File.WriteAllText (file, body)

    commit "Add the changelog entry" [ changelogFile ]
    push plan
    gh [ "pr" ; "edit" ; string<int> pullRequest ; "--body-file" ; file ]
    File.Delete file

    // Events authenticated with GITHUB_TOKEN do not create workflow runs, so opening the pull
    // request did not start CI. workflow_dispatch is the documented exception.
    gh [ "workflow" ; "run" ; "ci.yml" ; "--ref" ; plan.Branch ]

    closeSuperseded pullRequest

// ---------------------------------------------------------------------------------------------

match pendingBump () with
| NothingToDo reason -> printfn $"Nothing to do: %s{reason}"
| Pending plan ->

// For fantomas, which Format.format shells out to.
dotnet [ "tool" ; "restore" ]

let changes, drift = bumpVersions plan

// A no-op when the replacements above were as surgical as they are meant to be, and the difference
// between a green CI run on the bot's own pull request and a red one if not.
Format.format ()

// The changelog entry links the pull request it lands in, so it cannot be written before that pull
// request exists. Hence the second commit, and hence the body arriving after the fact.
let pullRequest = openPullRequest plan
let release, date, section = writeChangelog plan pullRequest
finishPullRequest plan pullRequest (renderBody plan changes drift release date section)
