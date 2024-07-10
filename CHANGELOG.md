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
