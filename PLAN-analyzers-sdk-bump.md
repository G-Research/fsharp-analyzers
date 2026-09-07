# Plan: scheduled FSharp.Analyzers.SDK bump PR

Working document for a later session. Not committed. Everything here was settled in a
design conversation on 2026-09-03; the "why" notes exist so the next session does not
relitigate decisions.

## Goal

A daily cron workflow that opens a PR with a first attempt at bumping
`FSharp.Analyzers.SDK`, so a maintainer can review, watch CI, and merge. Upstream
releases usually carry an FCS bump, which forces a minor version here, and that is more
than Dependabot can reason about. The bot will not always be right; the point is that the
starting point exists.

## Non-goals

- Auto-merging. A human presses the button.
- Handling upstream API breaks. When CI fails, the branch is a starting point for a human.
- Bumping anything else in `Directory.Packages.props` (NUnit, KeepAChangelog.Tasks, ...).
  Note in passing: Dependabot here only covers `github-actions`, so nothing watches the
  NuGet deps at all. Separate problem.

## Prerequisites

1. **Formatting pipeline** (done, see `format.fsx` + `build.fsx`). The bot's edits must
   land already-formatted or `checkFormat` fails CI on the bot's own PR.
2. **Trusted publishing** (done, see `NUGET-TRUSTED-PUBLISHING.md`). Not a hard blocker
   for the bot, but you do not want a near-automated release path still gated on a
   long-lived API key.

## Version discovery

Source of truth for "newest release" is the GitHub releases API, cross-checked against
NuGet:

```
gh api repos/ionide/FSharp.Analyzers.SDK/releases/latest --jq '.tag_name'   # -> v0.37.2
```

Strip the leading `v`. Then **both** of these must exist before proceeding, because a tag
can land minutes before the packages are indexed:

```
https://api.nuget.org/v3-flatcontainer/fsharp.analyzers.sdk/<version>/fsharp.analyzers.sdk.nuspec
https://api.nuget.org/v3-flatcontainer/fsharp.analyzers.sdk.testing/index.json
```

If the GitHub release exists but NuGet does not have the package yet, **log the reason and
exit 0**. It gets picked up tomorrow. A daily cron has no deadline, and a red X in the
Actions tab every time Ionide releases would train everyone to ignore the tab.

Also exit 0 when `Directory.Packages.props` already holds the latest version. That also
covers the case where upstream's `releases/latest` is somehow older than what is checked
in, so the bot never opens a downgrade PR.

## Deriving FSharp.Core and FSharp.Compiler.Service

Read them from the SDK's nuspec `<dependencies>` group for `net8.0`:

```xml
<dependency id="FSharp.Compiler.Service" version="43.12.201" exclude="Build,Analyzers" />
<dependency id="FSharp.Core" version="[10.1.201]" exclude="Build,Analyzers" />
```

Write both into `Directory.Packages.props` as exact pins, `[43.12.201]` and `[10.1.201]`,
wrapping in brackets regardless of whether the nuspec range was exact.

**Known drift, and the first PR will look odd because of it.** As of writing, the checked-in
pins are `FSharp.Core [10.0.101]` and `FSharp.Compiler.Service [43.10.101]`, which are
0.36.0's numbers, while 0.37.2 declares `43.12.201` and `[10.1.201]`. The bot's first PR
will therefore correct two lines that look unrelated to the version it is bumping. That is
correct behaviour, and the PR body should call it out explicitly.

**Why the drift went unnoticed, worth knowing before "fixing" it.** Both `PackageVersion`
lines are currently inert. `PackageVersion` only binds to a package the project has a
`PackageReference` item for. Neither fsproj has `PackageReference Include="FSharp.Compiler.Service"`.
For `FSharp.Core`, both fsprojs use `PackageReference Update="FSharp.Core"`, and `Update`
modifies an existing item rather than creating one; since `DisableImplicitFSharpCoreReference`
is `true`, the implicit reference the F# SDK would add is not there, so `Update` matches
nothing. Transitive deps are not governed by `PackageVersion` unless
`CentralPackageTransitivePinningEnabled` is set, and it is not. Verified: `dotnet restore`
resolves `FSharp.Core/10.1.201` and `FSharp.Compiler.Service/43.12.201` straight from the
SDK's nuspec, ignoring the file.

Decision taken: write accurate numbers, leave the binding question for later. Making the
pins load-bearing (`CentralPackageTransitivePinningEnabled`) is a real improvement, because
it turns drift into a restore error instead of a stale line, but it is a separate change to
`main` with its own risk and deserves its own PR.

## Files the bot edits

1. `Directory.Packages.props`
   - `FSharp.Analyzers.SDK` -> `[<version>]`
   - `FSharp.Analyzers.SDK.Testing` -> `[<version>]`
   - `FSharp.Core` -> `[<from nuspec>]`
   - `FSharp.Compiler.Service` -> `[<from nuspec>]`
2. `.config/dotnet-tools.json`, the `fsharp-analyzers` tool `version` -> `<version>`.
   Leave `fantomas` and `fsdocs-tool` alone.
3. `CHANGELOG.md`, see below.

Use surgical string replacement on the specific attribute and field values, **not** a DOM
round-trip, so the diff is exactly the lines that changed. Then let `format.fsx` normalise,
which is a no-op if the edits were surgical.

## Changelog

Version is a **minor bump off the topmost released version**, patch zeroed: `0.23.0` ->
`0.24.0`. Every SDK bump in this repo's history has been a minor regardless of what else
shipped, so there is no case where reusing the top section is right. Date is the run's UTC
date.

Entry format, matching the ten hand-written entries above it, with the upstream release
linked:

```markdown
- Update FSharp.Analyzers.SDK to [`0.24.0`](https://github.com/ionide/FSharp.Analyzers.SDK/releases/tag/v0.24.0). [#112](https://github.com/G-Research/fsharp-analyzers/pull/112)
```

Only the SDK is mentioned. FCS and FSharp.Core follow mechanically and are visible in the
diff.

### The `[Unreleased]` case

`## [Unreleased]` and `## Unreleased` have both appeared in this file historically. The
maintainers' pattern (see commits `8755a21` and `b260df5`) is to **promote** it: rename the
header in place to `## [x.y.z] - YYYY-MM-DD` and append the new entry into it, keeping the
existing lines. Copy that.

So:

- Top section is `Unreleased` -> rewrite its header to the new version and date, append the
  SDK line under its `### Changed` subsection, creating `### Changed` if absent, keeping
  every existing line.
- No `Unreleased` section -> insert a fresh `## [x.y.z] - date` / `### Changed` block at the
  top.

Consequence to accept: promoting `Unreleased` means the bot's PR releases someone else's
pending work alongside the bump. That is what the humans did too, and the PR body surfaces
those lines so the reviewer sees it, but it does mean a bot PR cannot be merged blind when
`Unreleased` has content.

Read the file with two regexes (topmost `## [x.y.z]` header, and whether the top section is
`Unreleased`). Do **not** reach for `Ionide.KeepAChangelog`: the 0.2.0 parser package
exports only `Parser` and `Domain`, no writer, and the `ToMarkdown` in the repo is an
extension on `ChangelogSubSectionCollection` that renders one section's `###` subsections
and bullets only, with no `##` header and no document. It also ships solely in
`Ionide.KeepAChangelog.Tasks`, which has no `lib/` folder (only `build/*.targets` and
`tasks/net472`, `tasks/net8.0`), so it is not referenceable via `PackageReference`.

## Workflow shape

`.github/workflows/bump-analyzers-sdk.yml`:

```yaml
name: Bump FSharp.Analyzers.SDK

on:
  schedule:
    - cron: "23 6 * * *"
  workflow_dispatch:

permissions:
  contents: write
  pull-requests: write
  actions: write        # needed to dispatch CI, see below
```

Steps:

1. `actions/checkout` (SHA-pinned, matching the repo's existing style with a `# vX.Y.Z`
   trailing comment).
2. `actions/setup-dotnet` (needed for `dotnet fsi`).
3. Resolve latest version, exit early if nothing to do.
4. `dotnet fsi bump-sdk.fsx <version>` for the props + tools.json edits.
5. `peter-evans/create-pull-request` -> creates branch and PR, outputs `pull-request-number`.
6. `dotnet fsi bump-sdk.fsx --changelog <version> <pr-number>` for the changelog.
7. `peter-evans/create-pull-request` again on the same branch -> second commit, PR updated.
8. `gh workflow run ci.yml --ref <branch>` to make CI actually run.
9. Close superseded bot PRs.

Implementation language is F# (`bump-sdk.fsx`, run with `dotnet fsi`), matching `build.fsx`
and `format.fsx`, and debuggable locally in a way inline YAML is not. **The script lives at
the repo root and will be linted by `checkFormat`**, so run `dotnet fsi build.fsx -p Format`
after writing it.

## Why two commits

The changelog entry contains the PR's own number, which does not exist until the PR does.
Hence: commit 1 (props + tools.json) -> create PR -> read the number -> commit 2 (changelog
with the real link) -> same branch, PR updated in place.

## Making CI actually run

This is the part that is easy to get wrong. **Events triggered by `GITHUB_TOKEN` do not
create workflow runs**, with the documented exception of `workflow_dispatch` and
`repository_dispatch`. So neither the PR creation nor the branch push will start `ci.yml`,
and adding a `push:` trigger for the bump branch would not help either.

The fix: add `workflow_dispatch` to `ci.yml`, give the bump job `actions: write`, and after
the PR is created run:

```
gh workflow run ci.yml --ref bump/analyzers-sdk-<version>
```

That dispatch is allowed with `github.token`. The resulting check runs attach to the branch
head SHA, which is the PR head, so they surface on the PR.

Verified: `main` has **no branch protection and no rulesets** (`rules/branches/main` and
`rulesets` both return `[]`), so there are no required status checks that a dispatch run
might fail to satisfy, and nothing blocks the merge button. If protection is added later,
recheck this.

## Branch, title, labels

- Branch: `bump/analyzers-sdk-<version>`, e.g. `bump/analyzers-sdk-0.38.0`.
  (Repo, package and upstream all spell it "analyzers".)
- Commit and PR title: `Update FSharp.Analyzers.SDK to 0.38.0`, matching the existing
  history. This repo does not use conventional commits; do not import them.
- Labels: `dependencies` and `automated`. `dependencies` exists; `automated` does not yet.
  Either create it by hand or have the workflow run `gh label create automated --force`
  first, because `create-pull-request` fails the step on a nonexistent label.
- `delete-branch: true`.

## Superseded PRs

Because the branch is versioned, a new version means a new branch and a new PR. Before
creating one, list open PRs with the `automated` label whose head branch starts with
`bump/analyzers-sdk-`, and for any that is not the current version, comment
"superseded by #N" and close it. One open bot PR at a time keeps the review queue honest,
and the old one is strictly superseded.

## PR body

- Link to the upstream release notes.
- Table of every version changed, old -> new.
- The full set of lines from the new changelog section, so promoted `Unreleased` content is
  visible to the reviewer.
- An explicit warning when the previous FCS or FSharp.Core pins disagreed with the *old*
  SDK version's nuspec, i.e. the drift case. Cheap to compute, and the single most useful
  triage signal the bot can produce.

## Open items

- Create the `automated` label, or add the `gh label create` step.
- Decide separately whether `CentralPackageTransitivePinningEnabled` should be turned on.
