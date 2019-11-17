{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes
  ( -- * Types
    Bytes
    -- * Properties
  , null
  , length
    -- * Filtering
  , takeWhile
  , dropWhile
  , takeWhileEnd
  , dropWhileEnd
    -- * Folds
  , foldl
  , foldl'
  , foldr
  , foldr'
    -- * Common Folds
  , elem
    -- * Splitting
  , Byte.split
  , Byte.split1
  , Byte.splitInit
  , splitFirst
    -- * Counting
  , Byte.count
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
  , unsafeIndex
    -- * Copying
  , copy
    -- * Pointers
  , pin
  , contents
  , touch
    -- * Conversion
  , toByteArray
  , toByteArrayClone
  , fromAsciiString
  , fromByteArray
  , toLatinString
  ) where

import Prelude hiding (length,takeWhile,dropWhile,null,foldl,foldr,elem)

import Control.Monad.Primitive (PrimMonad,PrimState,primitive_,unsafeIOToPrim)
import Control.Monad.ST.Run (runByteArrayST)
import Control.Monad.ST (runST)
import Data.Bytes.Types (Bytes(Bytes,array,offset))
import Data.Char (ord)
import Data.Primitive (ByteArray(ByteArray),MutableByteArray)
import GHC.Exts (Int(I#),Char(C#),word2Int#,chr#)
import GHC.Exts (Word#,Int#)
import GHC.Word (Word8(W8#))
import Foreign.Ptr (Ptr,plusPtr)

import qualified Data.Primitive as PM
import qualified Data.Bytes.Byte as Byte
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

-- | Split a byte sequence on the first occurrence of the target
-- byte. The target is removed from the result. For example:
--
-- >>> splitOnce 0xA [0x1,0x2,0xA,0xB]
-- Just ([0x1,0x2],[0xB])
splitFirst :: Word8 -> Bytes -> Maybe (Bytes,Bytes)
{-# inline splitFirst #-}
splitFirst w b@(Bytes arr off len) = case elemIndexLoop# w b of
  (-1#) -> Nothing
  i# -> let i = I# i# in
    Just (Bytes arr off (i - off), Bytes arr (i + 1) (len - (1 + i - off)))

-- This returns the offset into the byte array. This is not an index
-- that will mean anything to the end user, so it cannot be returned
-- to them.
elemIndexLoop# :: Word8 -> Bytes -> Int#
elemIndexLoop# !w (Bytes arr off@(I# off# ) len) = case len of
  0 -> (-1#)
  _ -> if PM.indexByteArray arr off == w
    then off#
    else elemIndexLoop# w (Bytes arr (off + 1) (len - 1))

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

-- | Copy the byte sequence into a mutable buffer. The buffer must have
-- enough space to accomodate the byte sequence, but this this is not
-- checked.
copy :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ Destination
  -> Int -- ^ Destination Offset
  -> Bytes -- ^ Source
  -> m ()
{-# inline copy #-}
copy dst dstIx (Bytes src srcIx len) =
  PM.copyByteArray dst dstIx src srcIx len

-- | Yields a pinned byte sequence whose contents are identical to those
-- of the original byte sequence. If the @ByteArray@ backing the argument
-- was already pinned, this simply aliases the argument and does not perform
-- any copying.
pin :: Bytes -> Bytes
pin b@(Bytes arr _ len) = case PM.isByteArrayPinned arr of
  True -> b
  False -> runST $ do
    dst <- PM.newPinnedByteArray len
    copy dst 0 b
    r <- PM.unsafeFreezeByteArray dst
    pure (Bytes r 0 len)

-- | Yields a pointer to the beginning of the byte sequence. It is only safe
-- to call this on a 'Bytes' backed by a pinned @ByteArray@.
contents :: Bytes -> Ptr Word8
contents (Bytes arr off _) = plusPtr (PM.byteArrayContents arr) off

-- | Touch the byte array backing the byte sequence. This sometimes needed
-- after calling 'contents' so that the @ByteArray@ does not get garbage
-- collected.
touch :: PrimMonad m => Bytes -> m ()
touch (Bytes (ByteArray arr) _ _) = unsafeIOToPrim
  (primitive_ (\s -> Exts.touch# arr s))
