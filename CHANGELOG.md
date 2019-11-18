# Revision history for byteslice

## 0.2.0.0 -- ????-??-??

* Change behavior of `split`. This function previously had a special case
  for zero-length byte sequences to mirror the behavior `bytestring`'s
  behavior. Now, `split` returns a singleton list with the empty byte
  sequence in this case.
* Add `split1` so that users who need to take advantage of the non-null
  guarantee split provides can do so.
* Make the C code compile on platforms that do not have `rawmemchr`.
* Rename `splitOnce` to `splitFirst`.
* Add `splitTwice`.

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
