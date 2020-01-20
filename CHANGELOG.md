# Revision history for byteslice

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
* Add `fromBytes` and `fromByteArray` to `Data.Bytes.Chunks`.
* Add `hGetContents` to `Data.Bytes.Chunks`.
* Add `isBytePrefixOf` and `isByteSuffixOf`.
* Add `replicate` and `replicateU`.
* Add `Monoid` instance for `Bytes`.
* Add `singleton`, `doubleton`, `tripleton`, and their unsliced variants.

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
