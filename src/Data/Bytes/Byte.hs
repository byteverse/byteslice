{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language UnliftedFFITypes #-}
{-# language UnboxedTuples #-}

-- This internal module has functions for splitting strings
-- on a particular byte and for counting occurences of that
-- byte.
module Data.Bytes.Byte
  ( count
  , split
  , splitInit
  ) where

import Prelude hiding (length)

import GHC.IO (unsafeIOToST)
import Data.Primitive (PrimArray(..),MutablePrimArray(..),ByteArray(..))
import Data.Word (Word8)
import GHC.Exts (Int(I#),ByteArray#,MutableByteArray#,Int#,Word#)
import GHC.Word (Word8(W8#))
import Data.Bytes.Types (Bytes(..))
import Control.Monad.ST.Run (runPrimArrayST)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | Count the number of times the byte appears in the sequence.
count :: Word8 -> Bytes -> Int
count !b (Bytes{array=ByteArray arr,offset,length}) =
  count_ba arr offset length b

-- | Break a byte sequence into pieces separated by the byte argument,
-- consuming the delimiter. This function is a good producer for list
-- fusion. It is common to immidiately consume the results of @split@
-- with @foldl'@, @traverse_@, @foldlM@, and being a good producer helps
-- in this situation.
split :: Word8 -> Bytes -> [Bytes]
{-# inline split #-}
split !w !bs@Bytes{array,offset=arrIx0} = Exts.build
  (\g x0 ->
    let go !lenIx !arrIx = if lenIx < lensSz
          then let !len = PM.indexPrimArray lens lenIx in 
            g (Bytes array arrIx len) (go (lenIx + 1) (arrIx + len + 1))
          else x0
     in go 0 arrIx0
  )
  where
  !lens = splitLengths w bs
  !lensSz = PM.sizeofPrimArray lens

-- | Variant of 'split' that drops the trailing element. This behaves
-- correctly even if the byte sequence is empty.
splitInit :: Word8 -> Bytes -> [Bytes]
{-# inline splitInit #-}
splitInit !w !bs@Bytes{array,offset=arrIx0} = Exts.build
  (\g x0 ->
    let go !lenIx !arrIx = if lenIx < lensSz
          then let !len = PM.indexPrimArray lens lenIx in 
            g (Bytes array arrIx len) (go (lenIx + 1) (arrIx + len + 1))
          else x0
     in go 0 arrIx0
  )
  where
  -- Remember, the resulting array from splitLengthsAlt always has
  -- a length of at least one.
  !lens = splitLengthsAlt w bs
  !lensSz = PM.sizeofPrimArray lens - 1

splitLengths :: Word8 -> Bytes -> PrimArray Int
{-# inline splitLengths #-}
splitLengths (W8# b) Bytes{array=ByteArray arr,offset=I# off,length=I# len} =
  PrimArray (splitLengths# b arr off len)

-- Internal function. This is just like splitLengths except that
-- it does not treat the empty byte sequences specially. The result
-- for that byte sequence is a singleton array with the element zero.
splitLengthsAlt :: Word8 -> Bytes -> PrimArray Int
splitLengthsAlt b Bytes{array=ByteArray arr#,offset=off,length=len} = runPrimArrayST do
  let !n = count_ba arr# off len b
  dst@(MutablePrimArray dst# ) :: MutablePrimArray s Int <- PM.newPrimArray (n + 1)
  total <- unsafeIOToST (memchr_ba_many arr# off dst# n b)
  PM.writePrimArray dst n (len - total)
  PM.unsafeFreezePrimArray dst

-- Internal function. When the argument is the empty byte sequence, this
-- returns an empty array of offsets.
splitLengths# :: Word# -> ByteArray# -> Int# -> Int# -> ByteArray#
{-# noinline splitLengths# #-}
splitLengths# b# arr# off# len# = result
  where
  b = W8# b#
  off = I# off#
  len = I# len#
  -- We make 0 a special case for compatibility with the behavior
  -- that the bytestring library provides.
  !(PrimArray result) = runPrimArrayST $ case len of
    0 -> do
      marr <- PM.newByteArray 0 
      ByteArray res# <- PM.unsafeFreezeByteArray marr
      pure (PrimArray res# )
    _ -> do
      let !n = count_ba arr# off len b
      dst@(MutablePrimArray dst# ) :: MutablePrimArray s Int <- PM.newPrimArray (n + 1)
      total <- unsafeIOToST (memchr_ba_many arr# off dst# n b)
      PM.writePrimArray dst n (len - total)
      PM.unsafeFreezePrimArray dst

foreign import ccall unsafe "bs_custom.h memchr_ba_many" memchr_ba_many
  :: ByteArray# -> Int -> MutableByteArray# s -> Int -> Word8 -> IO Int

foreign import ccall unsafe "bs_custom.h count_ba" count_ba
  :: ByteArray# -> Int -> Int -> Word8 -> Int
