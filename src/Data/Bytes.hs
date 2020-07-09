{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes
  ( -- * Types
    Bytes
    -- * Constants
  , Pure.empty
  , Pure.emptyPinned
  , Pure.emptyPinnedU
    -- * Properties
  , null
  , Pure.length
    -- * Decompose
  , uncons
  , unsnoc
    -- * Predicates
  , any
  , all
    -- * Create
    -- ** Sliced
  , singleton
  , doubleton
  , tripleton
  , replicate
    -- ** Unsliced
  , singletonU
  , doubletonU
  , tripletonU
  , replicateU
    -- * Filtering
  , takeWhile
  , dropWhile
  , takeWhileEnd
  , dropWhileEnd
    -- * Folds
  , foldl
  , Pure.foldl'
  , foldr
  , foldr'
    -- * Folds with Indices
  , ifoldl'
    -- * Common Folds
  , elem
    -- * Splitting
  , Byte.split
  , Byte.splitU
  , Byte.splitInit
  , Byte.splitInitU
  , Byte.splitNonEmpty
  , Byte.splitStream
  , Byte.split1
  , Byte.split2
  , Byte.split3
  , Byte.split4
    -- * Combining
  , intercalate
    -- * Counting
  , Byte.count
    -- * Prefix and Suffix
    -- ** Byte Sequence
  , isPrefixOf
  , isSuffixOf
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
    -- ** C Strings
  , equalsCString
    -- * Hashing
  , Pure.fnv1a32
  , Pure.fnv1a64
    -- * Unsafe Slicing
  , unsafeTake
  , unsafeDrop
  , unsafeIndex
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
  , toLatinString
  , fromCString#
  , toByteString
  , fromByteString
  , fromShortByteString
  , toShortByteString
  , toShortByteStringClone
    -- * I\/O with Handles
  , BIO.hGet
  , readFile
  , BIO.hPut
  ) where

import Prelude hiding (length,takeWhile,dropWhile,null,foldl,foldr,elem,replicate,any,all,readFile)

import Control.Monad.Primitive (PrimMonad,primitive_,unsafeIOToPrim)
import Control.Monad.ST.Run (runByteArrayST)
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes.Compat (cstringLength#)
import Data.Bytes.Pure (length,fromByteArray)
import Data.Bytes.Types (Bytes(Bytes,array,offset))
import Data.Char (ord)
import Data.Primitive (ByteArray(ByteArray))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr,plusPtr,castPtr)
import GHC.Exts (Int(I#),Char(C#),Ptr(Ptr),word2Int#,chr#)
import GHC.Exts (Addr#,Word#,Int#)
import GHC.IO (unsafeIOToST)
import GHC.Word (Word8(W8#))

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Bytes.Byte as Byte
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.IO as BIO
import qualified Data.Bytes.Pure as Pure
import qualified Data.Foldable as F
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM
import qualified GHC.Exts as Exts
import qualified GHC.ForeignPtr as ForeignPtr

-- | Is the byte sequence empty?
null :: Bytes -> Bool
null (Bytes _ _ len) = len == 0

-- | Extract the head and tail of the 'Bytes', returning 'Nothing' if
-- it is empty.
uncons :: Bytes -> Maybe (Word8, Bytes)
uncons b = case length b of
  0 -> Nothing
  _ -> Just (unsafeIndex b 0, unsafeDrop 1 b)

-- | Extract the @init@ and @last@ of the 'Bytes', returning 'Nothing' if
-- it is empty.
unsnoc :: Bytes -> Maybe (Bytes, Word8)
unsnoc b@(Bytes arr off len) = case len of
  0 -> Nothing
  _ -> let !len' = len - 1 in
    Just (Bytes arr off len', unsafeIndex b len')

-- | Does the byte sequence begin with the given byte? False if the
-- byte sequence is empty.
isBytePrefixOf :: Word8 -> Bytes -> Bool
isBytePrefixOf w b = case length b of
  0 -> False
  _ -> unsafeIndex b 0 == w

-- | Does the byte sequence end with the given byte? False if the
-- byte sequence is empty.
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
      && unsafeIndex a into == unsafeIndex b into
      = loop (into + 1)
    | otherwise = unsafeTake into a
  maxLen = min (length a) (length b)

-- | Create a byte sequence with one byte.
singleton :: Word8 -> Bytes
singleton !a = Bytes (singletonU a) 0 1

-- | Create a byte sequence with two bytes.
doubleton :: Word8 -> Word8 -> Bytes
doubleton !a !b = Bytes (doubletonU a b) 0 2

-- | Create a byte sequence with three bytes.
tripleton :: Word8 -> Word8 -> Word8 -> Bytes
tripleton !a !b !c = Bytes (tripletonU a b c) 0 3

-- | Create an unsliced byte sequence with one byte.
singletonU :: Word8 -> ByteArray
singletonU !a = runByteArrayST do
  arr <- PM.newByteArray 1
  PM.writeByteArray arr 0 a
  PM.unsafeFreezeByteArray arr

-- | Create an unsliced byte sequence with two bytes.
doubletonU :: Word8 -> Word8 -> ByteArray
doubletonU !a !b = runByteArrayST do
  arr <- PM.newByteArray 2
  PM.writeByteArray arr 0 a
  PM.writeByteArray arr 1 b
  PM.unsafeFreezeByteArray arr

-- | Create an unsliced byte sequence with three bytes.
tripletonU :: Word8 -> Word8 -> Word8 -> ByteArray
tripletonU !a !b !c = runByteArrayST do
  arr <- PM.newByteArray 3
  PM.writeByteArray arr 0 a
  PM.writeByteArray arr 1 b
  PM.writeByteArray arr 2 c
  PM.unsafeFreezeByteArray arr

-- | Replicate a byte @n@ times.
replicate ::
     Int -- ^ Desired length @n@
  -> Word8 -- ^ Byte to replicate
  -> Bytes
replicate !n !w = Bytes (replicateU n w) 0 n

-- | Variant of 'replicate' that returns a unsliced byte array.
replicateU :: Int -> Word8 -> ByteArray
replicateU !n !w = runByteArrayST do
  arr <- PM.newByteArray n
  PM.setByteArray arr 0 n w
  PM.unsafeFreezeByteArray arr

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
stripPrefix :: Bytes -> Bytes -> Maybe Bytes
stripPrefix !pre !str = if pre `isPrefixOf` str
  then Just (Bytes (array str) (offset str + length pre) (length str - length pre))
  else Nothing

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string. Otherwise, return the second
-- string unchanged.
stripOptionalPrefix :: Bytes -> Bytes -> Bytes
stripOptionalPrefix !pre !str = if pre `isPrefixOf` str
  then Bytes (array str) (offset str + length pre) (length str - length pre)
  else str

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
stripSuffix :: Bytes -> Bytes -> Maybe Bytes
stripSuffix !suf !str = if suf `isSuffixOf` str
  then Just (Bytes (array str) (offset str) (length str - length suf))
  else Nothing

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string. Otherwise, return the second
-- string unchanged.
stripOptionalSuffix :: Bytes -> Bytes -> Bytes
stripOptionalSuffix !suf !str = if suf `isSuffixOf` str
  then Bytes (array str) (offset str) (length str - length suf)
  else str

-- | Is the byte a member of the byte sequence?
elem :: Word8 -> Bytes -> Bool
elem (W8# w) b = case elemLoop 0# w b of
  1# -> True
  _ -> False

elemLoop :: Int# -> Word# -> Bytes -> Int#
elemLoop !r !w (Bytes arr@(ByteArray arr# ) off@(I# off# ) len) = case len of
  0 -> r
  _ -> elemLoop (Exts.orI# r (Exts.eqWord# w (Exts.indexWord8Array# arr# off# ) )) w (Bytes arr (off + 1) (len - 1))

-- | Take bytes while the predicate is true.
takeWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline takeWhile #-}
takeWhile k b = unsafeTake (countWhile k b) b

-- | Drop bytes while the predicate is true.
dropWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline dropWhile #-}
dropWhile k b = unsafeDrop (countWhile k b) b

-- | Index into the byte sequence at the given position. This index
-- must be less than the length.
unsafeIndex :: Bytes -> Int -> Word8
unsafeIndex (Bytes arr off _) ix = PM.indexByteArray arr (off + ix)

-- | /O(n)/ 'dropWhileEnd' @p@ @b@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
dropWhileEnd :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline dropWhileEnd #-}
dropWhileEnd k !b = unsafeTake (length b - countWhileEnd k b) b

-- | /O(n)/ 'takeWhileEnd' @p@ @b@ returns the longest suffix of
-- elements that satisfy predicate @p@.
takeWhileEnd :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline takeWhileEnd #-}
takeWhileEnd k !b =
  let n = countWhileEnd k b
   in Bytes (array b) (offset b + length b - n) n

-- | Take the first @n@ bytes from the argument. Precondition: @n ≤ len@
unsafeTake :: Int -> Bytes -> Bytes
{-# inline unsafeTake #-}
unsafeTake n (Bytes arr off _) =
  Bytes arr off n

-- | Drop the first @n@ bytes from the argument. Precondition: @n ≤ len@
unsafeDrop :: Int -> Bytes -> Bytes
{-# inline unsafeDrop #-}
unsafeDrop n (Bytes arr off len) =
  Bytes arr (off + n) (len - n)

-- Internal. The returns the number of bytes that match the
-- predicate until the first non-match occurs. If all bytes
-- match the predicate, this will return the length originally
-- provided.
countWhile :: (Word8 -> Bool) -> Bytes -> Int
{-# inline countWhile #-}
countWhile k (Bytes arr off0 len0) = go off0 len0 0 where
  go !off !len !n = if len > 0
    then if k (PM.indexByteArray arr off)
      then go (off + 1) (len - 1) (n + 1)
      else n
    else n

-- Internal. Variant of countWhile that starts from the end
-- of the string instead of the beginning.
countWhileEnd :: (Word8 -> Bool) -> Bytes -> Int
{-# inline countWhileEnd #-}
countWhileEnd k (Bytes arr off0 len0) = go (off0 + len0 - 1) (len0 - 1) 0 where
  go !off !len !n = if len >= 0
    then if k (PM.indexByteArray arr off)
      then go (off - 1) (len - 1) (n + 1)
      else n
    else n

-- | Left fold over bytes, non-strict in the accumulator.
foldl :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl #-}
foldl f a0 (Bytes arr off0 len0) =
  go (off0 + len0 - 1) (len0 - 1) 
  where
  go !off !ix = case ix of
    (-1) -> a0
    _ -> f (go (off - 1) (ix - 1)) (PM.indexByteArray arr off)

-- | Right fold over bytes, non-strict in the accumulator.
foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
{-# inline foldr #-}
foldr f a0 (Bytes arr off0 len0) = go off0 len0 where
  go !off !len = case len of
    0 -> a0
    _ -> f (PM.indexByteArray arr off) (go (off + 1) (len - 1))

-- | Left fold over bytes, strict in the accumulator. The reduction function
-- is applied to each element along with its index.
ifoldl' :: (a -> Int -> Word8 -> a) -> a -> Bytes -> a
{-# inline ifoldl' #-}
ifoldl' f a0 (Bytes arr off0 len0) = go a0 0 off0 len0 where
  go !a !ix !off !len = case len of
    0 -> a
    _ -> go (f a ix (PM.indexByteArray arr off)) (ix + 1) (off + 1) (len - 1)

-- | Right fold over bytes, strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> Bytes -> a
{-# inline foldr' #-}
foldr' f a0 (Bytes arr off0 len0) =
  go a0 (off0 + len0 - 1) (len0 - 1) 
  where
  go !a !off !ix = case ix of
    (-1) -> a
    _ -> go (f (PM.indexByteArray arr off) a) (off - 1) (ix - 1)

-- | Convert a 'String' consisting of only characters in the ASCII block
-- to a byte sequence. Any character with a codepoint above @U+007F@ is
-- replaced by @U+0000@.
fromAsciiString :: String -> Bytes
fromAsciiString = fromByteArray
  . Exts.fromList
  . map (\c -> let i = ord c in if i < 128 then fromIntegral @Int @Word8 i else 0)

-- | Convert a 'String' consisting of only characters representable
-- by ISO-8859-1. These are encoded with ISO-8859-1. Any character
-- with a codepoint above @U+00FF@ is replaced by an unspecified byte.
fromLatinString :: String -> Bytes
fromLatinString =
  fromByteArray . Exts.fromList . map (fromIntegral @Int @Word8 . ord)

-- | Interpret a byte sequence as text encoded by ISO-8859-1.
toLatinString :: Bytes -> String
toLatinString = foldr (\(W8# w) xs -> C# (chr# (word2Int# w)) : xs) []

-- | Copy a primitive string literal into managed memory.
fromCString# :: Addr# -> Bytes
fromCString# a = Bytes
  ( runByteArrayST $ do
      dst@(PM.MutableByteArray dst# ) <- PM.newByteArray len
      PM.copyPtrToMutablePrimArray
        (PM.MutablePrimArray dst# ) 0 (Ptr a :: Ptr Word8) len
      PM.unsafeFreezeByteArray dst
  ) 0 len
  where
  len = I# (cstringLength# a)

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (Exts.compareByteArrays# ba1# off1# ba2# off2# n#)) 0

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a singleton whose element matches the character?
equalsLatin1 :: Char -> Bytes -> Bool
equalsLatin1 !c0 (Bytes arr off len) = case len of
  1 -> c0 == indexCharArray arr off
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a doubleton whose elements match the characters?
equalsLatin2 :: Char -> Char -> Bytes -> Bool
equalsLatin2 !c0 !c1 (Bytes arr off len) = case len of
  2 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a tripleton whose elements match the characters?
equalsLatin3 :: Char -> Char -> Char -> Bytes -> Bool
equalsLatin3 !c0 !c1 !c2 (Bytes arr off len) = case len of
  3 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a quadrupleton whose elements match the characters?
equalsLatin4 :: Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin4 !c0 !c1 !c2 !c3 (Bytes arr off len) = case len of
  4 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a quintupleton whose elements match the characters?
equalsLatin5 :: Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin5 !c0 !c1 !c2 !c3 !c4 (Bytes arr off len) = case len of
  5 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3) &&
       c4 == indexCharArray arr (off + 4)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a sextupleton whose elements match the characters?
equalsLatin6 :: Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin6 !c0 !c1 !c2 !c3 !c4 !c5 (Bytes arr off len) = case len of
  6 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3) &&
       c4 == indexCharArray arr (off + 4) &&
       c5 == indexCharArray arr (off + 5)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a septupleton whose elements match the characters?
equalsLatin7 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin7 !c0 !c1 !c2 !c3 !c4 !c5 !c6 (Bytes arr off len) = case len of
  7 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3) &&
       c4 == indexCharArray arr (off + 4) &&
       c5 == indexCharArray arr (off + 5) &&
       c6 == indexCharArray arr (off + 6)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- an octupleton whose elements match the characters?
equalsLatin8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin8 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 (Bytes arr off len) = case len of
  8 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3) &&
       c4 == indexCharArray arr (off + 4) &&
       c5 == indexCharArray arr (off + 5) &&
       c6 == indexCharArray arr (off + 6) &&
       c7 == indexCharArray arr (off + 7)
  _ -> False

-- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
-- a 9-tuple whose elements match the characters?
equalsLatin9 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equalsLatin9 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 (Bytes arr off len) = case len of
  9 -> c0 == indexCharArray arr off &&
       c1 == indexCharArray arr (off + 1) &&
       c2 == indexCharArray arr (off + 2) &&
       c3 == indexCharArray arr (off + 3) &&
       c4 == indexCharArray arr (off + 4) &&
       c5 == indexCharArray arr (off + 5) &&
       c6 == indexCharArray arr (off + 6) &&
       c7 == indexCharArray arr (off + 7) &&
       c8 == indexCharArray arr (off + 8)
  _ -> False

-- | Is the byte sequence equal to the @NUL@-terminated C String?
-- The C string must be a constant.
equalsCString :: CString -> Bytes -> Bool
{-# inline equalsCString #-}
equalsCString !ptr0 (Bytes arr off0 len0) = go (castPtr ptr0 :: Ptr Word8) off0 len0 where
  go !ptr !off !len = case len of
    0 -> PM.indexOffPtr ptr 0 == (0 :: Word8)
    _ -> case PM.indexOffPtr ptr 0 of
      0 -> False
      c -> c == PM.indexByteArray arr off && go (plusPtr ptr 1) (off + 1) (len - 1)

-- | /O(n)/ Variant of 'stripPrefix' that takes a @NUL@-terminated C String
-- as the prefix to test for.
stripCStringPrefix :: CString -> Bytes -> Maybe Bytes
{-# inline stripCStringPrefix #-}
stripCStringPrefix !ptr0 (Bytes arr off0 len0) = go (castPtr ptr0 :: Ptr Word8) off0 len0 where
  go !ptr !off !len = case PM.indexOffPtr ptr 0 of
    0 -> Just (Bytes arr off len)
    c -> case len of
      0 -> Nothing
      _ -> case c == PM.indexByteArray arr off of
        True -> go (plusPtr ptr 1) (off + 1) (len - 1)
        False -> Nothing

-- | Touch the byte array backing the byte sequence. This sometimes needed
-- after calling 'contents' so that the @ByteArray@ does not get garbage
-- collected.
touch :: PrimMonad m => Bytes -> m ()
touch (Bytes (ByteArray arr) _ _) = unsafeIOToPrim
  (primitive_ (\s -> Exts.touch# arr s))

indexCharArray :: ByteArray -> Int -> Char
indexCharArray (ByteArray arr) (I# off) = C# (Exts.indexCharArray# arr off)

-- | Read an entire file strictly into a 'Bytes'.
readFile :: FilePath -> IO Bytes
readFile f = Chunks.concat <$> Chunks.readFile f

-- | /O(n)/ The intercalate function takes a separator 'Bytes' and a list of
-- 'Bytes' and concatenates the list elements by interspersing the separator
-- between each element.
intercalate ::
     Bytes -- ^ Separator (interspersed into the list)
  -> [Bytes] -- ^ List
  -> Bytes
intercalate !_ [] = mempty
intercalate !_ [x] = x
intercalate (Bytes sarr soff slen) (Bytes arr0 off0 len0 : bs) = Bytes r 0 fullLen
  where
  !fullLen = List.foldl' (\acc (Bytes _ _ len) -> acc + len + slen) 0 bs + len0
  r = runByteArrayST $ do
    marr <- PM.newByteArray fullLen
    PM.copyByteArray marr 0 arr0 off0 len0
    !_ <- F.foldlM
      (\ !currLen (Bytes arr off len) -> do
        PM.copyByteArray marr currLen sarr soff slen
        PM.copyByteArray marr (currLen + slen) arr off len
        pure (currLen + len + slen)
      ) len0 bs
    PM.unsafeFreezeByteArray marr

-- | /O(n)/ Returns true if any byte in the sequence satisfies the predicate.
any :: (Word8 -> Bool) -> Bytes -> Bool
{-# inline any #-}
any f = foldr (\b r -> f b || r) False

-- | /O(n)/ Returns true if all bytes in the sequence satisfy the predicate.
all :: (Word8 -> Bool) -> Bytes -> Bool
{-# inline all #-}
all f = foldr (\b r -> f b && r) True

-- | /O(n)/ when unpinned, /O(1)/ when pinned. Create a 'ByteString' from
-- a byte sequence. This only copies the byte sequence if it is not pinned.
toByteString :: Bytes -> ByteString
toByteString !b = pinnedToByteString (Pure.pin b)

-- | Convert the sliced 'Bytes' to an unsliced 'ShortByteString'. This
-- reuses the array backing the sliced 'Bytes' if the slicing metadata
-- implies that all of the bytes are used. Otherwise, it makes a copy.
toShortByteString :: Bytes -> ShortByteString
toShortByteString !b = case Pure.toByteArray b of
  PM.ByteArray x -> SBS x

-- | Variant of 'toShortByteString' that unconditionally makes a copy of
-- the array backing the sliced 'Bytes' even if the original array
-- could be reused. Prefer 'toShortByteString'.
toShortByteStringClone :: Bytes -> ShortByteString
toShortByteStringClone !b = case Pure.toByteArrayClone b of
  PM.ByteArray x -> SBS x

-- | /O(1)/ Create 'Bytes' from a 'ShortByteString'.
fromShortByteString :: ShortByteString -> Bytes
fromShortByteString (SBS x) = fromByteArray (ByteArray x)

-- | /O(n)/ Copy a 'ByteString' to a byte sequence.
fromByteString :: ByteString -> Bytes
fromByteString !b = Bytes
  ( runByteArrayST $ unsafeIOToST $ do 
      dst@(PM.MutableByteArray dst# ) <- PM.newByteArray len
      ByteString.unsafeUseAsCString b $ \src -> do
        PM.copyPtrToMutablePrimArray (PM.MutablePrimArray dst# ) 0 src len
      PM.unsafeFreezeByteArray dst
  ) 0 len
  where
  !len = ByteString.length b

-- Precondition: bytes are pinned
pinnedToByteString :: Bytes -> ByteString
pinnedToByteString (Bytes y@(PM.ByteArray x) off len) =
  ByteString.PS
    (ForeignPtr.ForeignPtr
      (case plusPtr (PM.byteArrayContents y) off of {Exts.Ptr p -> p})
      (ForeignPtr.PlainPtr (Exts.unsafeCoerce# x))
    )
    0 len
