# Changelog

## 0.9.1 - 2024-02-15

### Fixed
* Fixed a false negative of DisposedBeforeAsyncRunAnalyzer. [#75](https://github.com/G-Research/fsharp-analyzers/issues/75)

## 0.9.0 - 2024-02-15

### Removed

* Removed the PartialAppAnalyzer. [#68](https://github.com/G-Research/fsharp-analyzers/issues/68)

### Fixed
* Fixed a false positive of LoggingTemplateMissingValuesAnalyzer. [#69](https://github.com/G-Research/fsharp-analyzers/issues/69)

### Changed
* Update FSharp.Analyzers.SDK to `0.25.0`. [#51](https://github.com/G-Research/fsharp-analyzers/pull/67)

## 0.8.0 - 2024-01-30

### Fixed
* Add support for abbreviated list/array/set types to the VirtualCallAnalyzer. [#63](https://github.com/G-Research/fsharp-analyzers/pull/63)

### Added
* Disposable will be disposed before async is run. [#54](https://github.com/G-Research/fsharp-analyzers/issues/54)

### Changed
* Update FSharp.Analyzers.SDK to `0.24.0`. [#51](https://github.com/G-Research/fsharp-analyzers/pull/67)

## 0.7.0 - 2024-01-10

### Fixed
* Don't report FormattableStrings in TypedInterpolatedStringsAnalyzer. [#46](https://github.com/G-Research/fsharp-analyzers/pull/46)

### Added
* Add editor support to all analyzers. [#50](https://github.com/G-Research/fsharp-analyzers/pull/50)
* Add fix to VirtualCall Analyzer. [#51](https://github.com/G-Research/fsharp-analyzers/pull/51)
* Add fix to UnionCaseAnalyzer. [#51](https://github.com/G-Research/fsharp-analyzers/pull/51)
* Add new LoggingTemplateMissingValuesAnalyzer. [#53](https://github.com/G-Research/fsharp-analyzers/pull/53)

### Changed
* Update FSharp.Analyzers.SDK to `0.23.0`. [#51](https://github.com/G-Research/fsharp-analyzers/pull/45)

## 0.6.0 - 2023-12-20

### Added
* Add TypedInterpolatedStringsAnalyzer. [#40](https://github.com/G-Research/fsharp-analyzers/pull/40)

### Changed
* Update FSharp.Analyzers.SDK to `0.22.0`. [#27](https://github.com/G-Research/fsharp-analyzers/pull/45)

## 0.5.1 - 2023-12-06

### Fixed
* Handle types without a FullName more gracefully. [#39](https://github.com/G-Research/fsharp-analyzers/pull/39)

## 0.5.0 - 2023-12-04

### Added
* Add ImmutableCollectionEquality analyzer. [#37](https://github.com/G-Research/fsharp-analyzers/pull/37)
* Add LoggingArgFuncNotFullyApplied analyzer. [#38](https://github.com/G-Research/fsharp-analyzers/pull/38)

## 0.4.0 - 2023-11-23

### Added
* Add TypeAnnotateStringFunction analyzer. [#34](https://github.com/G-Research/fsharp-analyzers/pull/34)

### Changed
* Update FSharp.Analyzers.SDK to `0.21.0`. [#34](https://github.com/G-Research/fsharp-analyzers/pull/34)

## 0.3.1 - 2023-11-15

### Changed
* Reduced warnings in VirtualCallAnalyzer. [#31](https://github.com/G-Research/fsharp-analyzers/pull/31)
* Update FSharp.Analyzers.SDK to `0.20.2`. [#32](https://github.com/G-Research/fsharp-analyzers/pull/32)

## 0.3.0 - 2023-11-14

### Changed
* Update FSharp.Analyzers.SDK to `0.20.0`. [#27](https://github.com/G-Research/fsharp-analyzers/pull/29)

## 0.2.0 - 2023-11-10

### Changed
* Update FSharp.Analyzers.SDK to `0.18.0`. [#26](https://github.com/G-Research/fsharp-analyzers/pull/26)
* Update FSharp.Analyzers.SDK to `0.19.0`. [#27](https://github.com/G-Research/fsharp-analyzers/pull/27)

### Added
* Virtual call analyzer. [#27](https://github.com/G-Research/fsharp-analyzers/pull/27)

## [0.1.6] - 2023-12-26

### Changed
* Add more MSBuild properties to align with Roslyn. [#25](https://github.com/G-Research/fsharp-analyzers/pull/25)
* Update FSharp.Analyzers.SDK to `0.17.0`. [#25](https://github.com/G-Research/fsharp-analyzers/pull/25)

## [0.1.5] - 2023-10-17

### Fixed
* Partial application analyzer takes piped arguments into account.

## [0.1.4] - 2023-10-16

### Changed
* Update FSharp.Analyzers.SDK to `0.16.0`. [#22](https://github.com/G-Research/fsharp-analyzers/pull/22)

### Fixed
* String analyzers work for netstandard. [#19](https://github.com/G-Research/fsharp-analyzers/pull/19)

### Added
* Partial application analyzer. [#10](https://github.com/G-Research/fsharp-analyzers/pull/10)

## [0.1.3] - 2023-10-11
* Fix StringAnalyzers for netstandard projects

## [0.1.2] - 2023-10-10
* Update FSharp.Analyzers.SDK to `0.15.0`

## [0.1.1] - 2023-10-10

### Fixed
* Handle internal compiler exception regarding an unsupported kind of pattern match

## [0.1.0] - 2023-10-06

### Added
* Initial version