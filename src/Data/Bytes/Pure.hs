{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Pure
  ( empty
  , emptyPinned
  , pin
  , unsafeCopy
  , toByteArray
  , toByteArrayClone
  , fromByteArray
  , length
  , foldl'
  , fnv1a32
  , fnv1a64
  ) where

import Prelude hiding (length)

import Data.Bits (xor)
import Data.Word (Word64,Word32,Word8)
import Control.Monad.Primitive (PrimState,PrimMonad)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray,MutableByteArray)
import Control.Monad.ST.Run (runByteArrayST)

import qualified Data.Primitive as PM

-- | The empty byte sequence.
empty :: Bytes
empty = Bytes mempty 0 0

-- | The empty byte sequence.
emptyPinned :: Bytes
emptyPinned = Bytes
  ( runByteArrayST
    (PM.newPinnedByteArray 0 >>= PM.unsafeFreezeByteArray)
  ) 0 0

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

-- | Create a slice of 'Bytes' that spans the entire argument array.
fromByteArray :: ByteArray -> Bytes
fromByteArray b = Bytes b 0 (PM.sizeofByteArray b)

-- | The length of a slice of bytes.
length :: Bytes -> Int
length (Bytes _ _ len) = len

-- | Hash byte sequence with 32-bit variant of FNV-1a.
fnv1a32 :: Bytes -> Word32
fnv1a32 = foldl'
  (\acc w -> (fromIntegral @Word8 @Word32 w `xor` acc) * 0x01000193
  ) 0x811c9dc5

-- | Hash byte sequence with 64-bit variant of FNV-1a.
fnv1a64 :: Bytes -> Word64
fnv1a64 = foldl'
  (\acc w -> (fromIntegral @Word8 @Word64 w `xor` acc) * 0x00000100000001B3
  ) 0xcbf29ce484222325

-- | Left fold over bytes, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl' #-}
foldl' f a0 (Bytes arr off0 len0) = go a0 off0 len0 where
  go !a !off !len = case len of
    0 -> a
    _ -> go (f a (PM.indexByteArray arr off)) (off + 1) (len - 1)

