{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Data.Bytes
  ( -- * Types
    Bytes
    -- * Properties
  , null
  , length
    -- * Filtering
  , takeWhile
  , dropWhile
    -- * Folds
  , foldl
  , foldl'
  , foldr
  , foldr'
    -- * Prefix and Suffix
  , isPrefixOf
  , isSuffixOf
  , stripPrefix
  , stripOptionalPrefix
  , stripSuffix
  , stripOptionalSuffix
    -- * Unsafe Slicing
  , unsafeTake
  , unsafeDrop
    -- * Conversion
  , toByteArray
  , toByteArrayClone
  , fromAsciiString
  , fromByteArray
  , toLatinString
  ) where

import Prelude hiding (length,takeWhile,dropWhile,null,foldl,foldr)

import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes,array,offset))
import Data.Char (ord)
import Data.Primitive (ByteArray(ByteArray))
import GHC.Exts (Int(I#),Char(C#),word2Int#,chr#)
import GHC.Word (Word8(W8#))

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | Is the byte sequence empty?
null :: Bytes -> Bool
null (Bytes _ _ len) = len == 0

-- | The length of a slice of bytes.
length :: Bytes -> Int
length (Bytes _ _ len) = len

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

-- | Take bytes while the predicate is true.
takeWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline takeWhile #-}
takeWhile k b = unsafeTake (countWhile k b) b

-- | Drop bytes while the predicate is true.
dropWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline dropWhile #-}
dropWhile k b = unsafeDrop (countWhile k b) b

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

-- | Left fold over bytes, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl' #-}
foldl' f a0 (Bytes arr off0 len0) = go a0 off0 len0 where
  go !a !off !len = case len of
    0 -> a
    _ -> go (f a (PM.indexByteArray arr off)) (off + 1) (len - 1)

-- | Right fold over bytes, strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> Bytes -> a
{-# inline foldr' #-}
foldr' f a0 (Bytes arr off0 len0) =
  go a0 (off0 + len0 - 1) (len0 - 1) 
  where
  go !a !off !ix = case ix of
    (-1) -> a
    _ -> go (f (PM.indexByteArray arr off) a) (off - 1) (ix - 1)

-- | Convert the sliced 'Bytes' to an unsliced 'ByteArray'. This
-- reuses the array backing the sliced 'Bytes' if the slicing metadata
-- implies that all of the bytes are used. Otherwise, it makes a copy.
toByteArray :: Bytes -> ByteArray
toByteArray b@(Bytes arr off len)
  | off /= 0 = toByteArrayClone b
  | PM.sizeofByteArray arr /= len = toByteArrayClone b
  | otherwise = arr

-- | Variant of 'toByteArray' that unconditionally makes a copy of
-- the array backing the sliced 'Bytes' even if the original array
-- could be reused. Prefer 'toByteArray'.
toByteArrayClone :: Bytes -> ByteArray
toByteArrayClone (Bytes arr off len) = runByteArrayST $ do
  m <- PM.newByteArray len
  PM.copyByteArray m 0 arr off len
  PM.unsafeFreezeByteArray m

-- | Convert a 'String' consisting of only characters
--   in the ASCII block.
fromAsciiString :: String -> Bytes
fromAsciiString = fromByteArray . Exts.fromList . map (fromIntegral @Int @Word8 . ord)

-- | Interpret a byte sequence as text encoded by ISO-8859-1.
toLatinString :: Bytes -> String
toLatinString = foldr (\(W8# w) xs -> C# (chr# (word2Int# w)) : xs) []

-- | Create a slice of 'Bytes' that spans the entire argument array.
fromByteArray :: ByteArray -> Bytes
fromByteArray b = Bytes b 0 (PM.sizeofByteArray b)

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (Exts.compareByteArrays# ba1# off1# ba2# off2# n#)) 0
