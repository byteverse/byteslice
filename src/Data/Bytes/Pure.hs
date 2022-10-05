{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes.Pure
  ( empty
  , emptyPinned
  , emptyPinnedU
  , pin
  , contents
  , unsafeCopy
  , toByteArray
  , toByteArrayClone
  , toPinnedByteArray
  , toPinnedByteArrayClone
  , fromByteArray
  , fromPrimArray
  , length
  , foldlM
  , foldrM
  , foldl
  , foldl'
  , foldr
  , ifoldl'
  , foldr'
  , fnv1a32
  , fnv1a64
  , toByteString
  , pinnedToByteString
  , fromByteString
  , unsafeDrop
  ) where

import Prelude hiding (length,foldl,foldr)

import Control.Monad.Primitive (PrimState,PrimMonad)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (xor)
import Data.Bytes.Types (Bytes(Bytes))
import Data.ByteString (ByteString)
import Data.Primitive (ByteArray(ByteArray),MutableByteArray,PrimArray(PrimArray))
import Data.Word (Word64,Word32,Word8)
import Foreign.Ptr (Ptr,plusPtr)
import GHC.IO (unsafeIOToST)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified GHC.ForeignPtr as ForeignPtr

-- | The empty byte sequence.
empty :: Bytes
empty = Bytes mempty 0 0

-- | The empty pinned byte sequence.
emptyPinned :: Bytes
emptyPinned = Bytes
  ( runByteArrayST
    (PM.newPinnedByteArray 0 >>= PM.unsafeFreezeByteArray)
  ) 0 0

-- | The empty pinned byte sequence.
emptyPinnedU :: ByteArray
emptyPinnedU = runByteArrayST
  (PM.newPinnedByteArray 0 >>= PM.unsafeFreezeByteArray)

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
{-# inline toByteArray #-}
toByteArray b@(Bytes arr off len)
  | off == 0, PM.sizeofByteArray arr == len = arr
  | otherwise = toByteArrayClone b

-- | Variant of 'toByteArray' that unconditionally makes a copy of
-- the array backing the sliced 'Bytes' even if the original array
-- could be reused. Prefer 'toByteArray'.
toByteArrayClone :: Bytes -> ByteArray
{-# inline toByteArrayClone #-}
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
{-# inline fromByteArray #-}
fromByteArray b = Bytes b 0 (PM.sizeofByteArray b)

-- | Create a slice of 'Bytes' that spans the entire 'PrimArray' of 8-bit words.
fromPrimArray :: PrimArray Word8 -> Bytes
{-# inline fromPrimArray #-}
fromPrimArray p@(PrimArray b) = Bytes (ByteArray b) 0 (PM.sizeofPrimArray p)

-- | The length of a slice of bytes.
length :: Bytes -> Int
{-# inline length #-}
length (Bytes _ _ len) = len

-- | Hash byte sequence with 32-bit variant of FNV-1a.
fnv1a32 :: Bytes -> Word32
fnv1a32 !b = foldl'
  (\acc w -> (fromIntegral @Word8 @Word32 w `xor` acc) * 0x01000193
  ) 0x811c9dc5 b

-- | Hash byte sequence with 64-bit variant of FNV-1a.
fnv1a64 :: Bytes -> Word64
fnv1a64 !b = foldl'
  (\acc w -> (fromIntegral @Word8 @Word64 w `xor` acc) * 0x00000100000001B3
  ) 0xcbf29ce484222325 b

-- | Left fold over bytes, non-strict in the accumulator.
foldl :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl #-}
foldl f a0 (Bytes arr off0 len0) =
  go (off0 + len0 - 1) (len0 - 1) 
  where
  go !off !ix = case ix of
    (-1) -> a0
    _ -> f (go (off - 1) (ix - 1)) (PM.indexByteArray arr off)

-- | Left fold over bytes, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# inline foldl' #-}
foldl' f a0 (Bytes arr off0 len0) = go a0 off0 len0 where
  go !a !off !len = case len of
    0 -> a
    _ -> go (f a (PM.indexByteArray arr off)) (off + 1) (len - 1)

-- | Left monadic fold over bytes, non-strict in the accumulator.
foldlM :: Monad m => (a -> Word8 -> m a) -> a -> Bytes -> m a
{-# inline foldlM #-}
foldlM f a0 (Bytes arr off0 len0) = go a0 off0 len0 where
  go a !off !len = case len of
    0 -> pure a
    _ -> do
      a' <- f a (PM.indexByteArray arr off)
      go a' (off + 1) (len - 1)

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

-- | Right monadic fold over bytes, non-strict in the accumulator.
foldrM :: Monad m => (Word8 -> a -> m a) -> a -> Bytes -> m a
{-# inline foldrM #-}
foldrM f a0 (Bytes arr off0 len0) =
  go a0 (off0 + len0 - 1) (len0 - 1) 
  where
  go !a !off !ix = case ix of
    (-1) -> pure a
    _ -> do
      a' <- f (PM.indexByteArray arr off) a
      go a' (off - 1) (ix - 1)

-- | Yields a pointer to the beginning of the byte sequence. It is only safe
-- to call this on a 'Bytes' backed by a pinned @ByteArray@.
contents :: Bytes -> Ptr Word8
{-# inline contents #-}
contents (Bytes arr off _) = plusPtr (PM.byteArrayContents arr) off

-- | Convert the sliced 'Bytes' to an unsliced 'ByteArray'. This
-- reuses the array backing the sliced 'Bytes' if the slicing metadata
-- implies that all of the bytes are used and they are already pinned.
-- Otherwise, it makes a copy.
toPinnedByteArray :: Bytes -> ByteArray
{-# inline toPinnedByteArray #-}
toPinnedByteArray b@(Bytes arr off len)
  | off == 0, PM.sizeofByteArray arr == len, PM.isByteArrayPinned arr = arr
  | otherwise = toPinnedByteArrayClone b

-- | Variant of 'toPinnedByteArray' that unconditionally makes a copy of
-- the array backing the sliced 'Bytes' even if the original array
-- could be reused. Prefer 'toPinnedByteArray'.
toPinnedByteArrayClone :: Bytes -> ByteArray
toPinnedByteArrayClone (Bytes arr off len) = runByteArrayST $ do
  m <- PM.newPinnedByteArray len
  PM.copyByteArray m 0 arr off len
  PM.unsafeFreezeByteArray m

-- | /O(n)/ when unpinned, /O(1)/ when pinned. Create a 'ByteString' from
-- a byte sequence. This only copies the byte sequence if it is not pinned.
toByteString :: Bytes -> ByteString
toByteString !b = pinnedToByteString (pin b)

-- | Convert a pinned 'Bytes' to a 'ByteString'
-- /O(1)/ Precondition: bytes are pinned. Behavior is undefined otherwise.
pinnedToByteString :: Bytes -> ByteString
pinnedToByteString (Bytes y@(PM.ByteArray x) off len) =
  ByteString.PS
    (ForeignPtr.ForeignPtr
      (case plusPtr (PM.byteArrayContents y) off of {Exts.Ptr p -> p})
      (ForeignPtr.PlainPtr (Exts.unsafeCoerce# x))
    )
    0 len

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

-- | Drop the first @n@ bytes from the argument. Precondition: @n ≤ len@
unsafeDrop :: Int -> Bytes -> Bytes
{-# inline unsafeDrop #-}
unsafeDrop n (Bytes arr off len) =
  Bytes arr (off + n) (len - n)

