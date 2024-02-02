{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

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
  , fromLazyByteString
  , unsafeDrop
  , unsafeTake
  , unsafeIndex
  , unsafeHead
  , map
  , mapU
  , null
  , toShortByteString
  , replicate
  , replicateU
  ) where

import Prelude hiding (Foldable (..), map, replicate)

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (xor)
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (ByteArray), MutableByteArray, PrimArray (PrimArray))
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.IO (unsafeIOToST)

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Internal as LBS
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified GHC.ForeignPtr as ForeignPtr

-- | The empty byte sequence.
empty :: Bytes
empty = Bytes mempty 0 0

-- | The empty pinned byte sequence.
emptyPinned :: Bytes
emptyPinned =
  Bytes
    ( runByteArrayST
        (PM.newPinnedByteArray 0 >>= PM.unsafeFreezeByteArray)
    )
    0
    0

-- | The empty pinned byte sequence.
emptyPinnedU :: ByteArray
emptyPinnedU =
  runByteArrayST
    (PM.newPinnedByteArray 0 >>= PM.unsafeFreezeByteArray)

{- | Yields a pinned byte sequence whose contents are identical to those
of the original byte sequence. If the @ByteArray@ backing the argument
was already pinned, this simply aliases the argument and does not perform
any copying.
-}
pin :: Bytes -> Bytes
pin b@(Bytes arr _ len) = case PM.isByteArrayPinned arr of
  True -> b
  False ->
    Bytes
      ( runByteArrayST do
          dst <- PM.newPinnedByteArray len
          unsafeCopy dst 0 b
          PM.unsafeFreezeByteArray dst
      )
      0
      len

{- | Convert the sliced 'Bytes' to an unsliced 'ByteArray'. This
reuses the array backing the sliced 'Bytes' if the slicing metadata
implies that all of the bytes are used. Otherwise, it makes a copy.
-}
toByteArray :: Bytes -> ByteArray
{-# INLINE toByteArray #-}
toByteArray b@(Bytes arr off len)
  | off == 0, PM.sizeofByteArray arr == len = arr
  | otherwise = toByteArrayClone b

{- | Variant of 'toByteArray' that unconditionally makes a copy of
the array backing the sliced 'Bytes' even if the original array
could be reused. Prefer 'toByteArray'.
-}
toByteArrayClone :: Bytes -> ByteArray
{-# INLINE toByteArrayClone #-}
toByteArrayClone (Bytes arr off len) = runByteArrayST $ do
  m <- PM.newByteArray len
  PM.copyByteArray m 0 arr off len
  PM.unsafeFreezeByteArray m

{- | Copy the byte sequence into a mutable buffer. The buffer must have
enough space to accomodate the byte sequence, but this this is not
checked.
-}
unsafeCopy ::
  (PrimMonad m) =>
  -- | Destination
  MutableByteArray (PrimState m) ->
  -- | Destination Offset
  Int ->
  -- | Source
  Bytes ->
  m ()
{-# INLINE unsafeCopy #-}
unsafeCopy dst dstIx (Bytes src srcIx len) =
  PM.copyByteArray dst dstIx src srcIx len

-- | Create a slice of 'Bytes' that spans the entire argument array.
fromByteArray :: ByteArray -> Bytes
{-# INLINE fromByteArray #-}
fromByteArray b = Bytes b 0 (PM.sizeofByteArray b)

-- | Create a slice of 'Bytes' that spans the entire 'PrimArray' of 8-bit words.
fromPrimArray :: PrimArray Word8 -> Bytes
{-# INLINE fromPrimArray #-}
fromPrimArray p@(PrimArray b) = Bytes (ByteArray b) 0 (PM.sizeofPrimArray p)

-- | The length of a slice of bytes.
length :: Bytes -> Int
{-# INLINE length #-}
length (Bytes _ _ len) = len

-- | Hash byte sequence with 32-bit variant of FNV-1a.
fnv1a32 :: Bytes -> Word32
fnv1a32 !b =
  foldl'
    ( \acc w -> (fromIntegral @Word8 @Word32 w `xor` acc) * 0x01000193
    )
    0x811c9dc5
    b

-- | Hash byte sequence with 64-bit variant of FNV-1a.
fnv1a64 :: Bytes -> Word64
fnv1a64 !b =
  foldl'
    ( \acc w -> (fromIntegral @Word8 @Word64 w `xor` acc) * 0x00000100000001B3
    )
    0xcbf29ce484222325
    b

-- | Left fold over bytes, non-strict in the accumulator.
foldl :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# INLINE foldl #-}
foldl f a0 (Bytes arr off0 len0) =
  go (off0 + len0 - 1) (len0 - 1)
 where
  go !off !ix = case ix of
    (-1) -> a0
    _ -> f (go (off - 1) (ix - 1)) (PM.indexByteArray arr off)

-- | Left fold over bytes, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Bytes -> a
{-# INLINE foldl' #-}
foldl' f a0 (Bytes arr off0 len0) = go a0 off0 len0
 where
  go !a !off !len = case len of
    0 -> a
    _ -> go (f a (PM.indexByteArray arr off)) (off + 1) (len - 1)

-- | Left monadic fold over bytes, non-strict in the accumulator.
foldlM :: (Monad m) => (a -> Word8 -> m a) -> a -> Bytes -> m a
{-# INLINE foldlM #-}
foldlM f a0 (Bytes arr off0 len0) = go a0 off0 len0
 where
  go a !off !len = case len of
    0 -> pure a
    _ -> do
      a' <- f a (PM.indexByteArray arr off)
      go a' (off + 1) (len - 1)

-- | Right fold over bytes, non-strict in the accumulator.
foldr :: (Word8 -> a -> a) -> a -> Bytes -> a
{-# INLINE foldr #-}
foldr f a0 (Bytes arr off0 len0) = go off0 len0
 where
  go !off !len = case len of
    0 -> a0
    _ -> f (PM.indexByteArray arr off) (go (off + 1) (len - 1))

{- | Left fold over bytes, strict in the accumulator. The reduction function
is applied to each element along with its index.
-}
ifoldl' :: (a -> Int -> Word8 -> a) -> a -> Bytes -> a
{-# INLINE ifoldl' #-}
ifoldl' f a0 (Bytes arr off0 len0) = go a0 0 off0 len0
 where
  go !a !ix !off !len = case len of
    0 -> a
    _ -> go (f a ix (PM.indexByteArray arr off)) (ix + 1) (off + 1) (len - 1)

-- | Right fold over bytes, strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> Bytes -> a
{-# INLINE foldr' #-}
foldr' f a0 (Bytes arr off0 len0) =
  go a0 (off0 + len0 - 1) (len0 - 1)
 where
  go !a !off !ix = case ix of
    (-1) -> a
    _ -> go (f (PM.indexByteArray arr off) a) (off - 1) (ix - 1)

-- | Right monadic fold over bytes, non-strict in the accumulator.
foldrM :: (Monad m) => (Word8 -> a -> m a) -> a -> Bytes -> m a
{-# INLINE foldrM #-}
foldrM f a0 (Bytes arr off0 len0) =
  go a0 (off0 + len0 - 1) (len0 - 1)
 where
  go !a !off !ix = case ix of
    (-1) -> pure a
    _ -> do
      a' <- f (PM.indexByteArray arr off) a
      go a' (off - 1) (ix - 1)

{- | Yields a pointer to the beginning of the byte sequence. It is only safe
to call this on a 'Bytes' backed by a pinned @ByteArray@.
-}
contents :: Bytes -> Ptr Word8
{-# INLINE contents #-}
contents (Bytes arr off _) = plusPtr (PM.byteArrayContents arr) off

{- | Convert the sliced 'Bytes' to an unsliced 'ByteArray'. This
reuses the array backing the sliced 'Bytes' if the slicing metadata
implies that all of the bytes are used and they are already pinned.
Otherwise, it makes a copy.
-}
toPinnedByteArray :: Bytes -> ByteArray
{-# INLINE toPinnedByteArray #-}
toPinnedByteArray b@(Bytes arr off len)
  | off == 0, PM.sizeofByteArray arr == len, PM.isByteArrayPinned arr = arr
  | otherwise = toPinnedByteArrayClone b

{- | Variant of 'toPinnedByteArray' that unconditionally makes a copy of
the array backing the sliced 'Bytes' even if the original array
could be reused. Prefer 'toPinnedByteArray'.
-}
toPinnedByteArrayClone :: Bytes -> ByteArray
toPinnedByteArrayClone (Bytes arr off len) = runByteArrayST $ do
  m <- PM.newPinnedByteArray len
  PM.copyByteArray m 0 arr off len
  PM.unsafeFreezeByteArray m

{- | /O(n)/ when unpinned, /O(1)/ when pinned. Create a 'ByteString' from
a byte sequence. This only copies the byte sequence if it is not pinned.
-}
toByteString :: Bytes -> ByteString
toByteString !b = pinnedToByteString (pin b)

{- | Convert a pinned 'Bytes' to a 'ByteString'
/O(1)/ Precondition: bytes are pinned. Behavior is undefined otherwise.
-}
pinnedToByteString :: Bytes -> ByteString
pinnedToByteString (Bytes y@(PM.ByteArray x) off len) =
  ByteString.PS
    ( ForeignPtr.ForeignPtr
        (case plusPtr (PM.byteArrayContents y) off of Exts.Ptr p -> p)
        (ForeignPtr.PlainPtr (Exts.unsafeCoerce# x))
    )
    0
    len

-- | /O(n)/ Copy a 'ByteString' to a byte sequence.
fromByteString :: ByteString -> Bytes
fromByteString !b =
  Bytes
    ( runByteArrayST $ unsafeIOToST $ do
        dst@(PM.MutableByteArray dst#) <- PM.newByteArray len
        ByteString.unsafeUseAsCString b $ \src -> do
          PM.copyPtrToMutablePrimArray (PM.MutablePrimArray dst#) 0 src len
        PM.unsafeFreezeByteArray dst
    )
    0
    len
 where
  !len = ByteString.length b

-- | /O(n)/ Copy a lazy bytestring to a byte sequence.
fromLazyByteString :: LBS.ByteString -> Bytes
fromLazyByteString x = case LBS.length x of
  0 -> empty
  n64 ->
    let n = fromIntegral n64 :: Int
     in Bytes
          ( runByteArrayST $ unsafeIOToST $ do
              dst@(PM.MutableByteArray dst#) <- PM.newByteArray n
              let loop chunks !ix = case chunks of
                    LBS.Empty -> PM.unsafeFreezeByteArray dst
                    LBS.Chunk c cs -> do
                      let !len = ByteString.length c
                      ByteString.unsafeUseAsCString c $ \src -> do
                        PM.copyPtrToMutablePrimArray (PM.MutablePrimArray dst#) ix src len
                      loop cs (ix + len)
              loop x 0
          )
          0
          n

-- | Drop the first @n@ bytes from the argument. Precondition: @n ≤ len@
unsafeDrop :: Int -> Bytes -> Bytes
{-# INLINE unsafeDrop #-}
unsafeDrop n (Bytes arr off len) =
  Bytes arr (off + n) (len - n)

-- | Variant of 'map' that returns unsliced byte sequence.
mapU :: (Word8 -> Word8) -> Bytes -> ByteArray
{-# INLINE mapU #-}
mapU f (Bytes array ix0 len) = runByteArrayST do
  dst <- PM.newByteArray len
  let go !srcIx !dstIx =
        if dstIx < len
          then do
            let w = PM.indexByteArray array srcIx :: Word8
            PM.writeByteArray dst dstIx (f w)
            go (srcIx + 1) (dstIx + 1)
          else PM.unsafeFreezeByteArray dst
  go ix0 0

{- | Map over bytes in a sequence. The result has the same length as
the argument.
-}
map :: (Word8 -> Word8) -> Bytes -> Bytes
{-# INLINE map #-}
map f !b = Bytes (mapU f b) 0 (length b)

-- | Is the byte sequence empty?
null :: Bytes -> Bool
{-# INLINE null #-}
null (Bytes _ _ len) = len == 0

-- | Take the first @n@ bytes from the argument. Precondition: @n ≤ len@
unsafeTake :: Int -> Bytes -> Bytes
{-# INLINE unsafeTake #-}
unsafeTake n (Bytes arr off _) =
  Bytes arr off n

{- | Index into the byte sequence at the given position. This index
must be less than the length.
-}
unsafeIndex :: Bytes -> Int -> Word8
{-# INLINE unsafeIndex #-}
unsafeIndex (Bytes arr off _) ix = PM.indexByteArray arr (off + ix)

-- | Access the first byte. The given 'Bytes' must be non-empty.
{-# INLINE unsafeHead #-}
unsafeHead :: Bytes -> Word8
unsafeHead bs = unsafeIndex bs 0

{- | Convert the sliced 'Bytes' to an unsliced 'ShortByteString'. This
reuses the array backing the sliced 'Bytes' if the slicing metadata
implies that all of the bytes are used. Otherwise, it makes a copy.
-}
toShortByteString :: Bytes -> ShortByteString
{-# INLINE toShortByteString #-}
toShortByteString !b = case toByteArray b of
  PM.ByteArray x -> SBS x

-- | Replicate a byte @n@ times.
replicate ::
  -- | Desired length @n@
  Int ->
  -- | Byte to replicate
  Word8 ->
  Bytes
replicate !n !w = Bytes (replicateU n w) 0 n

-- | Variant of 'replicate' that returns a unsliced byte array.
replicateU :: Int -> Word8 -> ByteArray
replicateU !n !w = runByteArrayST do
  arr <- PM.newByteArray n
  PM.setByteArray arr 0 n w
  PM.unsafeFreezeByteArray arr
