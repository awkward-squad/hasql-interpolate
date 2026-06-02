## Unreleased

* Switch splice parser from `haskell-src-meta` to `ghc-hs-meta`. The previous parser silently misparsed `OverloadedRecordDot` syntax inside `#{...}` splices (e.g. `#{user.name}`) as the function-composition operator, leading to confusing type errors or wrong queries. The new parser uses GHC's own frontend with `OverloadedRecordDot`, `OverloadedLabels`, `OverloadedRecordUpdate`, and `TypeApplications` enabled, emitting `TH.GetFieldE` directly. Splice consumers using `#{x.y}` need `OverloadedRecordDot` enabled in their own module. Now requires GHC ≥ 9.2 (for `template-haskell-2.18`).

## [1.1.0.1] – April 14, 2026

* Fix bug in normalizing whitespace

## [1.1.0.0] – March 31, 2026

* Normalize whitespace - multiple consecutive whitespace characters are normalized to a single space.
* Support `hasql-1.10`

## [1.0.1.0] - July 16, 2024

* Add `DecodeValue` instance for `ByteString` and `LazyByteString`

## [1.0.0.0] - July 10, 2024

* Add IP address type encoders and decoders
* Increase tuple instances to size 16
* Support `hasql-1.8`

## [0.2.2.0] - May 7, 2024

* Make compile-time syntax error messages prettier
* Add `EncodeValue` instances for `ByteString` and `LazyByteString`
* Add `JsonBytes` and `JsonbBytes` newtypes

## [0.2.1.0] - August 29, 2023

* Fix encoder generation bug (https://github.com/awkward-squad/hasql-interpolate/pull/10)

## [0.2.0.0] - August 17, 2023

* Relax context of tuple instances for `EncodeRow` from `EncodeValue` to `EncodeField` (https://github.com/awkward-squad/hasql-interpolate/pull/9)

## [0.1.0.4] - January 10, 2023

* Support `mtl-2.3`

## [0.1.0.3] - July 31, 2022

* Support GHC 9.2

## [0.1.0.2] - February 4, 2022

* Support `hasql-1.5`

## [0.1.0.1] - November 15, 2021

* Fixed bug in multiline parser
