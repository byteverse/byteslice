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
  , empty
    -- * Properties
  , null
  , length
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
  , foldl'
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
  , split1
  , split2
  , split3
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
    -- ** Single Byte
  , isBytePrefixOf
  , isByteSuffixOf
    -- * Equality
  , equalsLatin1
  , equalsLatin2
  , equalsLatin3
  , equalsLatin4
  , equalsLatin5
  , equalsLatin6
  , equalsLatin7
    -- * Unsafe Slicing
  , unsafeTake
  , unsafeDrop
  , unsafeIndex
    -- * Copying
  , unsafeCopy
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
    -- * I\/O with Handles
  , hGet
  , hPut
  ) where

import Prelude hiding (length,takeWhile,dropWhile,null,foldl,foldr,elem,replicate)

import Control.Monad.Primitive (PrimMonad,PrimState,primitive_,unsafeIOToPrim)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes,array,offset))
import Data.Char (ord)
import Data.Primitive (ByteArray(ByteArray),MutableByteArray)
import Foreign.Ptr (Ptr,plusPtr)
import GHC.Exts (Int(I#),Char(C#),word2Int#,chr#)
import GHC.Exts (Word#,Int#)
import GHC.IO (IO(IO))
import GHC.Word (Word8(W8#))
import System.IO (Handle)

import qualified Data.Bytes.Byte as Byte
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified System.IO as IO

-- | Is the byte sequence empty?
null :: Bytes -> Bool
null (Bytes _ _ len) = len == 0

-- | The length of a slice of bytes.
length :: Bytes -> Int
length (Bytes _ _ len) = len

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

-- | Split a byte sequence on the first occurrence of the target
-- byte. The target is removed from the result. For example:
--
-- >>> split1 0xA [0x1,0x2,0xA,0xB]
-- Just ([0x1,0x2],[0xB])
split1 :: Word8 -> Bytes -> Maybe (Bytes,Bytes)
{-# inline split1 #-}
split1 w b@(Bytes arr off len) = case elemIndexLoop# w b of
  (-1#) -> Nothing
  i# -> let i = I# i# in
    Just (Bytes arr off (i - off), Bytes arr (i + 1) (len - (1 + i - off)))

-- | Split a byte sequence on the first and second occurrences
-- of the target byte. The target is removed from the result.
-- For example:
--
-- >>> split2 0xA [0x1,0x2,0xA,0xB,0xA,0xA,0xA]
-- Just ([0x1,0x2],[0xB],[0xA,0xA])
split2 :: Word8 -> Bytes -> Maybe (Bytes,Bytes,Bytes)
{-# inline split2 #-}
split2 w b@(Bytes arr off len) = case elemIndexLoop# w b of
  (-1#) -> Nothing
  i# -> let i = I# i# in
    case elemIndexLoop# w (Bytes arr (i + 1) (len - (1 + i - off))) of
      (-1#) -> Nothing
      j# -> let j = I# j# in Just
        ( Bytes arr off (i - off)
        , Bytes arr (i + 1) (j - (i + 1))
        , Bytes arr (j + 1) (len - (1 + j - off))
        )

-- | Split a byte sequence on the first, second, and third occurrences
-- of the target byte. The target is removed from the result.
-- For example:
--
-- >>> split3 0xA [0x1,0x2,0xA,0xB,0xA,0xA,0xA]
-- Just ([0x1,0x2],[0xB],[],[0xA])
split3 :: Word8 -> Bytes -> Maybe (Bytes,Bytes,Bytes,Bytes)
{-# inline split3 #-}
split3 w b@(Bytes arr off len) = case elemIndexLoop# w b of
  (-1#) -> Nothing
  i# -> let i = I# i# in
    case elemIndexLoop# w (Bytes arr (i + 1) (len - (1 + i - off))) of
      (-1#) -> Nothing
      j# -> let j = I# j# in
        case elemIndexLoop# w (Bytes arr (j + 1) (len - (1 + j - off))) of
          (-1#) -> Nothing
          k# -> let k = I# k# in Just
            ( Bytes arr off (i - off)
            , Bytes arr (i + 1) (j - (i + 1))
            , Bytes arr (j + 1) (k - (j + 1))
            , Bytes arr (k + 1) (len - (1 + k - off))
            )

-- This returns the offset into the byte array. This is not an index
-- that will mean anything to the end user, so it cannot be returned
-- to them.
elemIndexLoop# :: Word8 -> Bytes -> Int#
elemIndexLoop# !w (Bytes arr off@(I# off# ) len) = case len of
  0 -> (-1#)
  _ -> if PM.indexByteArray arr off == w
    then off#
    else elemIndexLoop# w (Bytes arr (off + 1) (len - 1))


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

-- | Left fold over bytes, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl' #-}
foldl' f a0 (Bytes arr off0 len0) = go a0 off0 len0 where
  go !a !off !len = case len of
    0 -> a
    _ -> go (f a (PM.indexByteArray arr off)) (off + 1) (len - 1)

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

-- | Convert the sliced 'Bytes' to an unsliced 'ByteArray'. This
-- reuses the array backing the sliced 'Bytes' if the slicing metadata
-- implies that all of the bytes are used. Otherwise, it makes a copy.
toByteArray :: Bytes -> ByteArray
toByteArray b@(Bytes arr off len)
  | off == 0, PM.sizeofByteArray arr == len = arr
  | otherwise = toByteArrayClone b

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

-- | Copy the byte sequence into a mutable buffer. The buffer must have
-- enough space to accomodate the byte sequence, but this this is not
-- checked.
unsafeCopy :: PrimMonad m
  => MutableByteArray (PrimState m) -- ^ Destination
  -> Int -- ^ Destination Offset
  -> Bytes -- ^ Source
  -> m ()
{-# inline unsafeCopy #-}
unsafeCopy dst dstIx (Bytes src srcIx len) =
  PM.copyByteArray dst dstIx src srcIx len

-- | Yields a pinned byte sequence whose contents are identical to those
-- of the original byte sequence. If the @ByteArray@ backing the argument
-- was already pinned, this simply aliases the argument and does not perform
-- any copying.
pin :: Bytes -> Bytes
pin b@(Bytes arr _ len) = case PM.isByteArrayPinned arr of
  True -> b
  False -> Bytes
    ( runByteArrayST do
        dst <- PM.newPinnedByteArray len
        unsafeCopy dst 0 b
        PM.unsafeFreezeByteArray dst
    ) 0 len

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

indexCharArray :: ByteArray -> Int -> Char
indexCharArray (ByteArray arr) (I# off) = C# (Exts.indexCharArray# arr off)

-- | The empty byte sequence.
empty :: Bytes
empty = Bytes mempty 0 0

-- | Read 'Bytes' directly from the specified 'Handle'. The resulting
-- 'Bytes' are pinned. This is implemented with 'hGetBuf'.
hGet :: Handle -> Int -> IO Bytes
hGet h i = createPinnedAndTrim i (\p -> IO.hGetBuf h p i)

-- | Outputs 'Bytes' to the specified 'Handle'. This is implemented
-- with 'hPutBuf'.
hPut :: Handle -> Bytes -> IO ()
hPut h b0 = do
  let b1@(Bytes arr _ len) = pin b0
  IO.hPutBuf h (contents b1) len
  touchByteArrayIO arr

-- Only used internally.
createPinnedAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO Bytes
{-# inline createPinnedAndTrim #-}
createPinnedAndTrim maxSz f = do
  arr@(PM.MutableByteArray arr#) <- PM.newPinnedByteArray maxSz
  sz <- f (PM.mutableByteArrayContents arr)
  PM.shrinkMutablePrimArray (PM.MutablePrimArray @Exts.RealWorld @Word8 arr#) sz
  r <- PM.unsafeFreezeByteArray arr
  pure (Bytes r 0 sz)

touchByteArrayIO :: ByteArray -> IO ()
touchByteArrayIO (ByteArray x) =
  IO (\s -> (# Exts.touch# x s, () #))
