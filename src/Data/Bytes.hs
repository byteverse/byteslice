{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}

{- | If you are interested in sub-arrays of 'ByteArray's (e.g. writing a binary
search), it would be grossly inefficient to make a copy of the sub-array. On
the other hand, it'd be really annoying to track limit indices by hand.

This module defines the 'Bytes' type which exposes a standard array interface
for a sub-arrays without copying and without manual index manipulation. --
For mutable arrays, see 'Data.Bytes.Mutable'.
-}
module Data.Bytes
  ( -- * Types
    Bytes

    -- * Constants
  , Pure.empty
  , Pure.emptyPinned
  , Pure.emptyPinnedU

    -- * Properties
  , Pure.null
  , Pure.length

    -- * Decompose
  , uncons
  , unsnoc

    -- * Predicates
  , Pure.any
  , Pure.all

    -- * Create

    -- ** Sliced
  , singleton
  , doubleton
  , tripleton
  , Pure.replicate

    -- ** Unsliced
  , singletonU
  , doubletonU
  , tripletonU
  , Pure.replicateU

    -- * Filtering
  , takeWhile
  , dropWhile
  , takeWhileEnd
  , dropWhileEnd

    -- * Traversals
  , Pure.map
  , Pure.mapU

    -- * Folds
  , Pure.foldl
  , Pure.foldl'
  , Pure.foldr
  , Pure.foldr'

    -- * Folds with Indices
  , Pure.ifoldl'

    -- * Monadic Folds
  , Pure.foldlM
  , Pure.foldrM

    -- * Common Folds
  , elem

    -- * Splitting

    -- ** Unlimited
  , Byte.split
  , Byte.splitU
  , Byte.splitInit
  , Byte.splitInitU
  , Byte.splitNonEmpty
  , Byte.splitStream

    -- ** Fixed from Beginning
  , Byte.split1
  , Pure.splitTetragram1
  , Byte.split2
  , Byte.split3
  , Byte.split4

    -- ** Fixed from End
  , Byte.splitEnd1

    -- * Combining
  , intercalate
  , intercalateByte2
  , concatArray
  , concatArrayU

    -- * Searching
  , replace
  , findIndices
  , Pure.findTetragramIndex

    -- * Counting
  , Byte.count

    -- * Prefix and Suffix

    -- ** Byte Sequence
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , stripPrefix
  , stripOptionalPrefix
  , stripSuffix
  , stripOptionalSuffix
  , longestCommonPrefix

    -- ** C Strings
  , stripCStringPrefix

    -- ** Single Byte
  , isBytePrefixOf
  , isByteSuffixOf

    -- * Equality

    -- ** Fixed Characters
  , equalsLatin1
  , equalsLatin2
  , equalsLatin3
  , equalsLatin4
  , equalsLatin5
  , equalsLatin6
  , equalsLatin7
  , equalsLatin8
  , equalsLatin9
  , equalsLatin10
  , equalsLatin11
  , equalsLatin12

    -- ** C Strings
  , equalsCString

    -- * Hashing
  , Pure.fnv1a32
  , Pure.fnv1a64

    -- * Unsafe Slicing
  , Pure.unsafeTake
  , Pure.unsafeDrop
  , Pure.unsafeIndex
  , Pure.unsafeHead

    -- * Copying
  , Pure.unsafeCopy

    -- * Pointers
  , Pure.pin
  , Pure.contents
  , touch

    -- * Conversion
  , Pure.toByteArray
  , Pure.toByteArrayClone
  , Pure.toPinnedByteArray
  , Pure.toPinnedByteArrayClone
  , fromAsciiString
  , fromLatinString
  , Pure.fromByteArray
  , Pure.fromPrimArray
  , toLatinString
  , fromCString#
  , Pure.toByteString
  , Pure.pinnedToByteString
  , Pure.fromByteString
  , Pure.fromLazyByteString
  , fromShortByteString
  , fromShortText
  , toShortByteString
  , toShortByteStringClone
  , toLowerAsciiByteArrayClone

    -- * I\/O with Handles
  , BIO.hGet
  , readFile
  , BIO.hPut

    -- * Unlifted Types
  , lift
  , unlift

    -- * Length Indexed
  , withLength
  , withLengthU
  ) where

import Prelude hiding (all, any, dropWhile, elem, foldl, foldr, length, map, null, readFile, replicate, takeWhile)

import Control.Monad.Primitive (PrimMonad, primitive_, unsafeIOToPrim)
import Control.Monad.ST.Run (runByteArrayST)
import Cstrlen (cstringLength#)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Bytes.Pure (fromByteArray, length, toShortByteString, unsafeDrop, unsafeIndex)
import Data.Bytes.Search (findIndices, isInfixOf, replace)
import Data.Bytes.Types (ByteArrayN (ByteArrayN), Bytes (Bytes, array, offset), BytesN (BytesN))
import Data.Primitive (Array, ByteArray (ByteArray))
import Data.Text.Short (ShortText)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Exts (Addr#, Int (I#), Int#, Ptr (Ptr), Word#)
import GHC.Word (Word8 (W8#))
import Reps (Bytes# (..), word8ToWord#)

import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Types as Arithmetic
import qualified Data.Bytes.Byte as Byte
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.IO as BIO
import qualified Data.Bytes.Pure as Pure
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Bytes.Text.AsciiExt as AsciiExt
import qualified Data.Bytes.Text.Latin1 as Latin1
import qualified Data.Bytes.Types as Types
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM
import qualified Data.Text.Short as TS
import qualified GHC.Exts as Exts
import qualified GHC.TypeNats as GHC

{- | Extract the head and tail of the 'Bytes', returning 'Nothing' if
it is empty.
-}
uncons :: Bytes -> Maybe (Word8, Bytes)
{-# INLINE uncons #-}
uncons b = case length b of
  0 -> Nothing
  _ -> Just (unsafeIndex b 0, unsafeDrop 1 b)

{- | Extract the @init@ and @last@ of the 'Bytes', returning 'Nothing' if
it is empty.
-}
unsnoc :: Bytes -> Maybe (Bytes, Word8)
{-# INLINE unsnoc #-}
unsnoc b@(Bytes arr off len) = case len of
  0 -> Nothing
  _ ->
    let !len' = len - 1
     in Just (Bytes arr off len', unsafeIndex b len')

{- | Does the byte sequence begin with the given byte? False if the
byte sequence is empty.
-}
isBytePrefixOf :: Word8 -> Bytes -> Bool
{-# INLINE isBytePrefixOf #-}
isBytePrefixOf w b = case length b of
  0 -> False
  _ -> unsafeIndex b 0 == w

{- | Does the byte sequence end with the given byte? False if the
byte sequence is empty.
-}
isByteSuffixOf :: Word8 -> Bytes -> Bool
isByteSuffixOf w b = case len of
  0 -> False
  _ -> unsafeIndex b (len - 1) == w
 where
  len = length b

-- | Is the first argument a prefix of the second argument?
isPrefixOf :: Bytes -> Bytes -> Bool
isPrefixOf (Bytes a aOff aLen) (Bytes b bOff bLen) =
  -- For prefix and suffix testing, we do not use
  -- the sameByteArray optimization that we use in
  -- the Eq instance. Prefix and suffix testing seldom
  -- compares a byte array with the same in-memory
  -- byte array.
  if aLen <= bLen
    then compareByteArrays a aOff b bOff aLen == EQ
    else False

-- | Is the first argument a suffix of the second argument?
isSuffixOf :: Bytes -> Bytes -> Bool
isSuffixOf (Bytes a aOff aLen) (Bytes b bOff bLen) =
  if aLen <= bLen
    then compareByteArrays a aOff b (bOff + bLen - aLen) aLen == EQ
    else False

-- | Find the longest string which is a prefix of both arguments.
longestCommonPrefix :: Bytes -> Bytes -> Bytes
longestCommonPrefix a b = loop 0
 where
  loop :: Int -> Bytes
  loop !into
    | into < maxLen
        && unsafeIndex a into == unsafeIndex b into =
        loop (into + 1)
    | otherwise = Pure.unsafeTake into a
  maxLen = min (length a) (length b)

-- | Create a byte sequence with one byte.
singleton :: Word8 -> Bytes
{-# INLINE singleton #-}
singleton !a = Bytes (singletonU a) 0 1

-- | Create a byte sequence with two bytes.
doubleton :: Word8 -> Word8 -> Bytes
{-# INLINE doubleton #-}
doubleton !a !b = Bytes (doubletonU a b) 0 2

-- | Create a byte sequence with three bytes.
tripleton :: Word8 -> Word8 -> Word8 -> Bytes
{-# INLINE tripleton #-}
tripleton !a !b !c = Bytes (tripletonU a b c) 0 3

-- | Create an unsliced byte sequence with one byte.
singletonU :: Word8 -> ByteArray
{-# INLINE singletonU #-}
singletonU !a = runByteArrayST do
  arr <- PM.newByteArray 1
  PM.writeByteArray arr 0 a
  PM.unsafeFreezeByteArray arr

-- | Create an unsliced byte sequence with two bytes.
doubletonU :: Word8 -> Word8 -> ByteArray
{-# INLINE doubletonU #-}
doubletonU !a !b = runByteArrayST do
  arr <- PM.newByteArray 2
  PM.writeByteArray arr 0 a
  PM.writeByteArray arr 1 b
  PM.unsafeFreezeByteArray arr

-- | Create an unsliced byte sequence with three bytes.
tripletonU :: Word8 -> Word8 -> Word8 -> ByteArray
{-# INLINE tripletonU #-}
tripletonU !a !b !c = runByteArrayST do
  arr <- PM.newByteArray 3
  PM.writeByteArray arr 0 a
  PM.writeByteArray arr 1 b
  PM.writeByteArray arr 2 c
  PM.unsafeFreezeByteArray arr

{- | /O(n)/ Return the suffix of the second string if its prefix
matches the entire first string.
-}
stripPrefix :: Bytes -> Bytes -> Maybe Bytes
stripPrefix !pre !str =
  if pre `isPrefixOf` str
    then Just (Bytes (array str) (offset str + length pre) (length str - length pre))
    else Nothing

{- | /O(n)/ Return the suffix of the second string if its prefix
matches the entire first string. Otherwise, return the second
string unchanged.
-}
stripOptionalPrefix :: Bytes -> Bytes -> Bytes
stripOptionalPrefix !pre !str =
  if pre `isPrefixOf` str
    then Bytes (array str) (offset str + length pre) (length str - length pre)
    else str

{- | /O(n)/ Return the prefix of the second string if its suffix
matches the entire first string.
-}
stripSuffix :: Bytes -> Bytes -> Maybe Bytes
stripSuffix !suf !str =
  if suf `isSuffixOf` str
    then Just (Bytes (array str) (offset str) (length str - length suf))
    else Nothing

{- | /O(n)/ Return the prefix of the second string if its suffix
matches the entire first string. Otherwise, return the second
string unchanged.
-}
stripOptionalSuffix :: Bytes -> Bytes -> Bytes
stripOptionalSuffix !suf !str =
  if suf `isSuffixOf` str
    then Bytes (array str) (offset str) (length str - length suf)
    else str

-- | Is the byte a member of the byte sequence?
elem :: Word8 -> Bytes -> Bool
elem (W8# w) b = case elemLoop 0# (word8ToWord# w) b of
  1# -> True
  _ -> False

elemLoop :: Int# -> Word# -> Bytes -> Int#
elemLoop !r !w (Bytes arr@(ByteArray arr#) off@(I# off#) len) = case len of
  0 -> r
  _ -> elemLoop (Exts.orI# r (Exts.eqWord# w (word8ToWord# (Exts.indexWord8Array# arr# off#)))) w (Bytes arr (off + 1) (len - 1))

-- | Take bytes while the predicate is true.
takeWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# INLINE takeWhile #-}
takeWhile k b = Pure.unsafeTake (Pure.countWhile k b) b

-- | Drop bytes while the predicate is true.
dropWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# INLINE dropWhile #-}
dropWhile k b = Pure.unsafeDrop (Pure.countWhile k b) b

{- | /O(n)/ 'dropWhileEnd' @p@ @b@ returns the prefix remaining after
dropping characters that satisfy the predicate @p@ from the end of
@t@.
-}
dropWhileEnd :: (Word8 -> Bool) -> Bytes -> Bytes
{-# INLINE dropWhileEnd #-}
dropWhileEnd k !b = Pure.unsafeTake (length b - Pure.countWhileEnd k b) b

{- | /O(n)/ 'takeWhileEnd' @p@ @b@ returns the longest suffix of
elements that satisfy predicate @p@.
-}
takeWhileEnd :: (Word8 -> Bool) -> Bytes -> Bytes
{-# INLINE takeWhileEnd #-}
takeWhileEnd k !b =
  let n = Pure.countWhileEnd k b
   in Bytes (array b) (offset b + length b - n) n

{- | Convert a 'String' consisting of only characters in the ASCII block
to a byte sequence. Any character with a codepoint above @U+007F@ is
replaced by @U+0000@.
-}
fromAsciiString :: String -> Bytes
{-# DEPRECATED fromAsciiString "use Data.Bytes.Text.Ascii.fromString instead" #-}
{-# INLINE fromAsciiString #-}
fromAsciiString = Ascii.fromString

{- | Convert a 'String' consisting of only characters representable
by ISO-8859-1. These are encoded with ISO-8859-1. Any character
with a codepoint above @U+00FF@ is replaced by an unspecified byte.
-}
fromLatinString :: String -> Bytes
{-# DEPRECATED fromLatinString "use Data.Bytes.Text.Latin1.fromString instead" #-}
{-# INLINE fromLatinString #-}
fromLatinString = Latin1.fromString

-- | Interpret a byte sequence as text encoded by ISO-8859-1.
toLatinString :: Bytes -> String
{-# DEPRECATED toLatinString "use Data.Bytes.Text.Latin1.toString instead" #-}
{-# INLINE toLatinString #-}
toLatinString = Latin1.toString

-- | Copy a primitive string literal into managed memory.
fromCString# :: Addr# -> Bytes
fromCString# a =
  Bytes
    ( runByteArrayST $ do
        dst@(PM.MutableByteArray dst#) <- PM.newByteArray len
        PM.copyPtrToMutablePrimArray
          (PM.MutablePrimArray dst#)
          0
          (Ptr a :: Ptr Word8)
          len
        PM.unsafeFreezeByteArray dst
    )
    0
    len
 where
  len = I# (cstringLength# a)

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (Exts.compareByteArrays# ba1# off1# ba2# off2# n#)) 0

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a singleton whose element matches the character?
-}
equalsLatin1 :: Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin1 "use Data.Bytes.Text.Latin1.equals1 instead" #-}
{-# INLINE equalsLatin1 #-}
equalsLatin1 = Latin1.equals1

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a doubleton whose elements match the characters?
-}
equalsLatin2 :: Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin2 "use Data.Bytes.Text.Latin1.equals2 instead" #-}
{-# INLINE equalsLatin2 #-}
equalsLatin2 = Latin1.equals2

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a tripleton whose elements match the characters?
-}
equalsLatin3 :: Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin3 "use Data.Bytes.Text.Latin1.equals3 instead" #-}
{-# INLINE equalsLatin3 #-}
equalsLatin3 = Latin1.equals3

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a quadrupleton whose elements match the characters?
-}
equalsLatin4 :: Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin4 "use Data.Bytes.Text.Latin1.equals4 instead" #-}
{-# INLINE equalsLatin4 #-}
equalsLatin4 = Latin1.equals4

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a quintupleton whose elements match the characters?
-}
equalsLatin5 :: Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin5 "use Data.Bytes.Text.Latin1.equals5 instead" #-}
{-# INLINE equalsLatin5 #-}
equalsLatin5 = Latin1.equals5

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a sextupleton whose elements match the characters?
-}
equalsLatin6 :: Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin6 "use Data.Bytes.Text.Latin1.equals6 instead" #-}
{-# INLINE equalsLatin6 #-}
equalsLatin6 = Latin1.equals6

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a septupleton whose elements match the characters?
-}
equalsLatin7 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin7 "use Data.Bytes.Text.Latin1.equals7 instead" #-}
{-# INLINE equalsLatin7 #-}
equalsLatin7 = Latin1.equals7

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
an octupleton whose elements match the characters?
-}
equalsLatin8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin8 "use Data.Bytes.Text.Latin1.equals8 instead" #-}
{-# INLINE equalsLatin8 #-}
equalsLatin8 = Latin1.equals8

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 9-tuple whose elements match the characters?
-}
equalsLatin9 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin9 "use Data.Bytes.Text.Latin1.equals9 instead" #-}
{-# INLINE equalsLatin9 #-}
equalsLatin9 = Latin1.equals9

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 10-tuple whose elements match the characters?
-}
equalsLatin10 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin10 "use Data.Bytes.Text.Latin1.equals10 instead" #-}
{-# INLINE equalsLatin10 #-}
equalsLatin10 = Latin1.equals10

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 11-tuple whose elements match the characters?
-}
equalsLatin11 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin11 "use Data.Bytes.Text.Latin1.equals11 instead" #-}
{-# INLINE equalsLatin11 #-}
equalsLatin11 = Latin1.equals11

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 12-tuple whose elements match the characters?
-}
equalsLatin12 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
{-# DEPRECATED equalsLatin12 "use Data.Bytes.Text.Latin1.equals12 instead" #-}
{-# INLINE equalsLatin12 #-}
equalsLatin12 = Latin1.equals12

{- | Is the byte sequence equal to the @NUL@-terminated C String?
The C string must be a constant.
-}
equalsCString :: CString -> Bytes -> Bool
{-# INLINE equalsCString #-}
equalsCString !ptr0 (Bytes arr off0 len0) = go (castPtr ptr0 :: Ptr Word8) off0 len0
 where
  go !ptr !off !len = case len of
    0 -> PM.indexOffPtr ptr 0 == (0 :: Word8)
    _ -> case PM.indexOffPtr ptr 0 of
      0 -> False
      c -> c == PM.indexByteArray arr off && go (plusPtr ptr 1) (off + 1) (len - 1)

{- | /O(n)/ Variant of 'stripPrefix' that takes a @NUL@-terminated C String
as the prefix to test for.
-}
stripCStringPrefix :: CString -> Bytes -> Maybe Bytes
{-# INLINE stripCStringPrefix #-}
stripCStringPrefix !ptr0 (Bytes arr off0 len0) = go (castPtr ptr0 :: Ptr Word8) off0 len0
 where
  go !ptr !off !len = case PM.indexOffPtr ptr 0 of
    0 -> Just (Bytes arr off len)
    c -> case len of
      0 -> Nothing
      _ -> case c == PM.indexByteArray arr off of
        True -> go (plusPtr ptr 1) (off + 1) (len - 1)
        False -> Nothing

{- | Touch the byte array backing the byte sequence. This sometimes needed
after calling 'Pure.contents' so that the @ByteArray@ does not get garbage
collected.
-}
touch :: (PrimMonad m) => Bytes -> m ()
touch (Bytes (ByteArray arr) _ _) =
  unsafeIOToPrim
    (primitive_ (\s -> Exts.touch# arr s))

-- | Read an entire file strictly into a 'Bytes'.
readFile :: FilePath -> IO Bytes
readFile f = Chunks.concat <$> Chunks.readFile f

{- | /O(n)/ The intercalate function takes a separator 'Bytes' and a list of
'Bytes' and concatenates the list elements by interspersing the separator
between each element.
-}
intercalate ::
  -- | Separator (interspersed into the list)
  Bytes ->
  -- | List
  [Bytes] ->
  Bytes
intercalate !_ [] = mempty
intercalate !_ [x] = x
intercalate (Bytes sarr soff slen) (Bytes arr0 off0 len0 : bs) = Bytes r 0 fullLen
 where
  !fullLen = List.foldl' (\acc (Bytes _ _ len) -> acc + len + slen) 0 bs + len0
  r = runByteArrayST $ do
    marr <- PM.newByteArray fullLen
    PM.copyByteArray marr 0 arr0 off0 len0
    !_ <-
      F.foldlM
        ( \ !currLen (Bytes arr off len) -> do
            PM.copyByteArray marr currLen sarr soff slen
            PM.copyByteArray marr (currLen + slen) arr off len
            pure (currLen + len + slen)
        )
        len0
        bs
    PM.unsafeFreezeByteArray marr

{- | Specialization of 'intercalate' where the separator is a single byte and
there are exactly two byte sequences that are being concatenated.
-}
intercalateByte2 ::
  -- | Separator
  Word8 ->
  -- | First byte sequence
  Bytes ->
  -- | Second byte sequence
  Bytes ->
  Bytes
intercalateByte2 !sep !a !b =
  Bytes
    { Types.array = runByteArrayST $ do
        dst <- PM.newByteArray len
        Pure.unsafeCopy dst 0 a
        PM.writeByteArray dst (length a) sep
        Pure.unsafeCopy dst (length a + 1) b
        PM.unsafeFreezeByteArray dst
    , Types.length = len
    , Types.offset = 0
    }
 where
  len = length a + length b + 1

{- | Variant of 'toShortByteString' that unconditionally makes a copy of
the array backing the sliced 'Bytes' even if the original array
could be reused. Prefer 'toShortByteString'.
-}
toShortByteStringClone :: Bytes -> ShortByteString
{-# INLINE toShortByteStringClone #-}
toShortByteStringClone !b = case Pure.toByteArrayClone b of
  PM.ByteArray x -> SBS x

-- | /O(1)/ Create 'Bytes' from a 'ShortByteString'.
fromShortByteString :: ShortByteString -> Bytes
{-# INLINE fromShortByteString #-}
fromShortByteString (SBS x) = fromByteArray (ByteArray x)

{- | /O(1)/ Create 'Bytes' from a 'ShortText'. This encodes the text as UTF-8.
It is a no-op.
-}
fromShortText :: ShortText -> Bytes
{-# INLINE fromShortText #-}
fromShortText t = case TS.toShortByteString t of
  SBS x -> fromByteArray (ByteArray x)

{- | /O(n)/ Interpreting the bytes an ASCII-encoded characters, convert
the string to lowercase. This adds @0x20@ to bytes in the range
@[0x41,0x5A]@ and leaves all other bytes alone. Unconditionally
copies the bytes.
-}
toLowerAsciiByteArrayClone :: Bytes -> ByteArray
{-# DEPRECATED toLowerAsciiByteArrayClone "use Data.Bytes/Text/AsciiExt.toLowerU" #-}
{-# INLINE toLowerAsciiByteArrayClone #-}
toLowerAsciiByteArrayClone = AsciiExt.toLowerU

lift :: Bytes# -> Bytes
{-# INLINE lift #-}
lift (Bytes# (# arr, off, len #)) = Bytes (ByteArray arr) (I# off) (I# len)

unlift :: Bytes -> Bytes#
{-# INLINE unlift #-}
unlift (Bytes (ByteArray arr) (I# off) (I# len)) =
  Bytes# (# arr, off, len #)

concatArrayU :: Array Bytes -> ByteArray
{-# NOINLINE concatArrayU #-}
concatArrayU !xs = runByteArrayST $ do
  let !arrLen = PM.sizeofArray xs
  let !totalByteLen = F.foldl' (\acc b -> length b + acc) 0 xs
  dst <- PM.newByteArray totalByteLen
  let go !ix !dstOff =
        if ix < arrLen
          then do
            x <- PM.indexArrayM xs ix
            Pure.unsafeCopy dst dstOff x
            go (ix + 1) (dstOff + length x)
          else PM.unsafeFreezeByteArray dst
  go 0 0

concatArray :: Array Bytes -> Bytes
{-# INLINE concatArray #-}
concatArray !xs = Pure.fromByteArray (concatArrayU xs)

{- | Convert 'Bytes' to 'BytesN', exposing the length in a type-safe
way in the callback.
-}
withLength ::
  Bytes ->
  (forall (n :: GHC.Nat). Arithmetic.Nat n -> BytesN n -> a) ->
  a
{-# INLINE withLength #-}
withLength Bytes {array, offset, length = len} f =
  Nat.with
    len
    (\n -> f n BytesN {array, offset})

withLengthU ::
  ByteArray ->
  (forall (n :: GHC.Nat). Arithmetic.Nat n -> ByteArrayN n -> a) ->
  a
{-# INLINE withLengthU #-}
withLengthU !arr f =
  Nat.with
    (PM.sizeofByteArray arr)
    (\n -> f n (ByteArrayN arr))
