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
  , splitU
  , splitNonEmpty
  , splitInit
  , splitInitU
  ) where

import Prelude hiding (length)

import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runPrimArrayST)
import Data.Bytes.Types (Bytes(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Primitive (PrimArray(..),MutablePrimArray(..),ByteArray(..))
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Word (Word8)
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.IO (unsafeIOToST)

import qualified Data.Primitive as PM
import qualified Data.Primitive.Unlifted.Array as PM
import qualified GHC.Exts as Exts

-- | Count the number of times the byte appears in the sequence.
count :: Word8 -> Bytes -> Int
count !b (Bytes{array=ByteArray arr,offset,length}) =
  count_ba arr offset length b

-- | Variant of 'split' that returns an array of unsliced byte sequences.
-- Unlike 'split', this is not a good producer for list fusion. (It does
-- not return a list, so it could not be.) Prefer 'split' if the result
-- is going to be consumed exactly once by a good consumer. Prefer 'splitU'
-- if the result of the split is going to be around for a while and
-- inspected multiple times.
splitU :: Word8 -> Bytes -> UnliftedArray ByteArray
splitU !w !bs =
  let !lens = splitLengthsAlt w bs
      !lensSz = PM.sizeofPrimArray lens
   in splitCommonU lens lensSz bs

-- | Variant of 'splitU' that drops the trailing element. See 'splitInit'
-- for an explanation of why this may be useful.
splitInitU :: Word8 -> Bytes -> UnliftedArray ByteArray
splitInitU !w !bs =
  let !lens = splitLengthsAlt w bs
      !lensSz = PM.sizeofPrimArray lens
   in splitCommonU lens (lensSz - 1) bs

-- Internal function
splitCommonU ::
     PrimArray Int -- array of segment lengths
  -> Int -- number of lengths to consider
  -> Bytes
  -> UnliftedArray ByteArray
splitCommonU !lens !lensSz Bytes{array,offset=arrIx0} = runST do
  dst <- PM.unsafeNewUnliftedArray lensSz
  let go !lenIx !arrIx = if lenIx < lensSz
        then do
          let !len = PM.indexPrimArray lens lenIx
          buf <- PM.newByteArray len
          PM.copyByteArray buf 0 array arrIx len
          buf' <- PM.unsafeFreezeByteArray buf
          PM.writeUnliftedArray dst lenIx buf'
          go (lenIx + 1) (arrIx + len + 1)
        else pure ()
  go 0 arrIx0
  PM.unsafeFreezeUnliftedArray dst
  

-- | Break a byte sequence into pieces separated by the byte argument,
-- consuming the delimiter. This function is a good producer for list
-- fusion. It is common to immidiately consume the results of @split@
-- with @foldl'@, @traverse_@, @foldlM@, and being a good producer helps
-- in this situation.
--
-- Note: this function differs from its counterpart in @bytestring@.
-- If the byte sequence is empty, this returns a singleton list with
-- the empty byte sequence.
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
  !lens = splitLengthsAlt w bs
  !lensSz = PM.sizeofPrimArray lens

-- | Variant of 'split' that returns the result as a 'NonEmpty'
-- instead of @[]@. This is also eligible for stream fusion.
splitNonEmpty :: Word8 -> Bytes -> NonEmpty Bytes
{-# inline splitNonEmpty #-}
splitNonEmpty !w !bs@Bytes{array,offset=arrIx0} =
  Bytes array arrIx0 len0 :| Exts.build
  (\g x0 ->
    let go !lenIx !arrIx = if lenIx < lensSz
          then let !len = PM.indexPrimArray lens lenIx in 
            g (Bytes array arrIx len) (go (lenIx + 1) (arrIx + len + 1))
          else x0
     in go 1 (1 + (arrIx0 + len0))
  )
  where
  !lens = splitLengthsAlt w bs
  !lensSz = PM.sizeofPrimArray lens
  !len0 = PM.indexPrimArray lens 0 :: Int

-- | Variant of 'split' that drops the trailing element. This behaves
-- correctly even if the byte sequence is empty. This is a good producer
-- for list fusion. This is useful when splitting a text file
-- into lines.
-- <https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap03.html#tag_03_392 POSIX>
-- mandates that text files end with a newline, so the list resulting
-- from 'split' always has an empty byte sequence as its last element.
-- With 'splitInit', that unwanted element is discarded.
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

-- Internal function. This is just like splitLengths except that
-- it does not treat the empty byte sequences specially. The result
-- for that byte sequence is a singleton array with the element zero.
splitLengthsAlt :: Word8 -> Bytes -> PrimArray Int
splitLengthsAlt b Bytes{array=ByteArray arr#,offset=off,length=len} = runPrimArrayST do
  let !n = count_ba arr# off len b
  dst@(MutablePrimArray dst# ) :: MutablePrimArray s Int <- PM.newPrimArray (n + 1)
  total <- unsafeIOToST (memchr_ba_many arr# off len dst# n b)
  PM.writePrimArray dst n (len - total)
  PM.unsafeFreezePrimArray dst

foreign import ccall unsafe "bs_custom.h memchr_ba_many" memchr_ba_many
  :: ByteArray# -> Int -> Int -> MutableByteArray# s -> Int -> Word8 -> IO Int

foreign import ccall unsafe "bs_custom.h count_ba" count_ba
  :: ByteArray# -> Int -> Int -> Word8 -> Int
