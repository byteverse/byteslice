# Revision history for byteslice

## 0.2.11.1 -- 2023-07-26

* Fix regression causing build failure in GHC 9.2.

## 0.2.11.0 -- 2023-07-25

* Add `Data.Bytes.Encode.LittleEndian`.
* Add `splitTetragram1`.

## 0.2.10.0 -- 2023-05-01

* Add `equals13`, `equals14`, `equals15`.
* Add `Show` instances for `BytesN` and `ByteArrayN`.
* Add `equalsCStringCaseInsensitive` for ascii text.

## 0.2.9.0 -- 2022-12-08

* Add `toShortText`, `toShortTextU`, and `toText` to `Data.Byte.Text.Ascii`.
* Add `fromShortText` and `fromText` to `Data.Bytes.Text.Utf8`.

## 0.2.8.0 -- 2022-11-21

* Add `Data.Bytes.replace` and `Data.Bytes.findIndices`
* Add `Data.Bytes.fromShortText`
* Add `Data.Bytes.fromPrimArray`
* Add `decodeDecWord`
* Add `concatArray` and `concatArrayU`.

## 0.2.7.0 -- 2022-02-16

* Add support for GHC 9.2.
* Drop support for GHC 8.8 and earlier.
* Add `foldlM` and `foldrM` for mondic folds over byte sequences.

## 0.2.6.0 -- 2021-09-15

* Add `BytesN` and `ByteArrayN`.
* Add `isInfixOf`.
* Add `hForLines_` and `hFoldLines`.
* Add `lift` and `unlift` for converting between `Bytes` and `Bytes#`.
* Move text-oriented functions from Data.Bytes to `Data.Bytes.Text.*`.
  Provide aliases with older names that come with deprecation warning.

## 0.2.5.2 -- 2021-02-23

* Correct compatibility shims.

## 0.2.5.1 -- 2021-02-22 (deprecated)

* Compatibility with GHC 9.0.

## 0.2.5.0 -- 2021-01-22

* Add `Data.Bytes.Chunks.concatByteString`.
* Expose `pinnedToByteString` to end users. 

## 0.2.4.0 -- 2020-10-15

* Add `toByteString` and `fromByteString`.
* Add `fromShortByteString`.
* Add `equalsLatin(9|10|11|12)`.
* Add `toPinnedByteArray`, `toPinnedByteArrayClone`, and `concatPinnedU`.
* Add `toLowerAsciiByteArrayClone`.
* Add `intercalateByte2`.
* Add `splitEnd1`.

## 0.2.3.0 -- 2020-04-30

* Add `fnv1a32` and `fnv1a64`, implementations of the 32-bit and
  64-bit variants of the FNV-1a hash algorithm, to both `Data.Bytes`
  and `Data.Bytes.Chunks`.
* Add `Data.Bytes.Chunks.null`.
* Add `readFile` to both `Data.Bytes` and `Data.Bytes.Chunks`.
* Add `foldl'` to `Data.Bytes.Chunks`.
* Add `split` to `Data.Bytes.Chunks`.
* Add `splitStream` for splitting as a good stream-fusion producer.
* Add `hPut` and `writeFile` to `Data.Bytes.Chunks`.
* Add `fromCString#`.
* Add `Bytes#` newtype on GHC 8.10 and up.

## 0.2.2.0 -- 2020-02-27

* Add `split4`.
* Add `equalsCString`.
* Add `stripCStringPrefix`.
* Add `equalsLatin8`.
* Add `emptyPinned`.
* Add `concatPinned` to `Data.Bytes.Chunks`.
* Add `any` and `all`.

## 0.2.1.0 -- 2020-01-22

* Add `longestCommonPrefix`.
* Fix broken `Ord` instance of `Bytes`.

## 0.2.0.0 -- 2020-01-20

* Change behavior of `split`. This function previously had a special case
  for zero-length byte sequences to mirror the behavior `bytestring`'s
  behavior. Now, `split` returns a singleton list with the empty byte
  sequence in this case.
* Add `splitNonEmpty` so that users who need to take advantage of the
  non-null guarantee `split` provides can do so.
* Add `splitU` and `splitInitU` for users who are going to split a
  byte sequence without and consume the results more than once.
* Make the C code compile on platforms that do not have `rawmemchr`.
* Rename `splitOnce` to `split1`.
* Add `split2` and `split3`.
* Add `equalsLatin{1,2,3,4,5,6,7}`
* Add `ifoldl'`.
* Add `hGet` and `hPut`.
* Move `Data.Bytes.Chunks` from `small-bytearray-builder` to `byteslice`.
* Rename `Data.Bytes.Chunks.concat` to `concatU` (the U means unsliced),
  and add a new `concat` that returns `Bytes`.
* Add `fromBytes`, `fromByteArray`, and `unsafeCopy` to `Data.Bytes.Chunks`.
* Add `hGetContents` to `Data.Bytes.Chunks`.
* Add `isBytePrefixOf` and `isByteSuffixOf`.
* Add `replicate` and `replicateU`.
* Add `Monoid` instance for `Bytes`.
* Add `singleton`, `doubleton`, `tripleton`, and their unsliced variants.
* Rename `copy` to `unsafeCopy`.
* Add `fromLatinString`.
* Change the behavior of `fromAsciiString` to replace out-of-bounds codepoints
  with NUL.
* Add `unsnoc` and `uncons`.

## 0.1.4.0 -- 2019-11-12

* Add `toLatinString`.
* Add `stripPrefix`, `stripSuffix`, `stripOptionalPrefix`, and
  `stripOptionalSuffix`.
* Add `takeWhileEnd` and `dropWhileEnd`.
* Add `count`.
* Add an optimized `split` function.
* Add `splitInit`.
* Add `splitFirst`.
* Add `copy`.
* Add `pin`.
* Add `touch`.
* Add `elem`.
* Add `unsafeIndex`.

## 0.1.3.0 -- 2019-09-15

* Add `isPrefixOf` and `isSuffixOf`.
* Add `foldl`, `foldr`, `foldl'`, and `foldr'`.

## 0.1.2.0 -- 2019-08-21

* Add `Data.Bytes.Mutable` module.
* Add `Data.Bytes` module.

## 0.1.1.0 -- 2019-07-03

* Add record labels for Bytes and MutableBytes
* Add UnmanagedBytes. This is just an Addr and a length.

## 0.1.0.0 -- 2019-04-30

* First version.
