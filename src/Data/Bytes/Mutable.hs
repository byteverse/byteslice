{-# language BangPatterns #-}
{-# language LambdaCase #-}

module Data.Bytes.Mutable
  ( -- * Types
    MutableBytes
    -- * Filtering
  , takeWhile
  , dropWhile
    -- * Unsafe Slicing
  , unsafeTake
  , unsafeDrop
    -- * Conversion
  , fromMutableByteArray
  ) where

import Prelude hiding (takeWhile,dropWhile)

import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Primitive (MutableByteArray)
import Data.Word (Word8)
import Control.Monad.Primitive (PrimMonad,PrimState)

import qualified Data.Primitive as PM

-- | Take bytes while the predicate is true, aliasing the
-- argument array.
takeWhile :: PrimMonad m
  => (Word8 -> m Bool)
  -> MutableBytes (PrimState m)
  -> m (MutableBytes (PrimState m))
{-# inline takeWhile #-}
takeWhile k b = do
  n <- countWhile k b
  pure (unsafeTake n b)

-- | Drop bytes while the predicate is true, aliasing the
-- argument array.
dropWhile :: PrimMonad m
  => (Word8 -> m Bool)
  -> MutableBytes (PrimState m)
  -> m (MutableBytes (PrimState m))
{-# inline dropWhile #-}
dropWhile k b = do
  n <- countWhile k b
  pure (unsafeDrop n b)

-- | Take the first @n@ bytes from the argument, aliasing it.
unsafeTake :: Int -> MutableBytes s -> MutableBytes s
{-# inline unsafeTake #-}
unsafeTake n (MutableBytes arr off _) =
  MutableBytes arr off n

-- | Drop the first @n@ bytes from the argument, aliasing it.
-- The new length will be @len - n@.
unsafeDrop :: Int -> MutableBytes s -> MutableBytes s
{-# inline unsafeDrop #-}
unsafeDrop n (MutableBytes arr off len) =
  MutableBytes arr (off + n) (len - n)

-- | Create a slice of 'MutableBytes' that spans the entire
-- argument array. This aliases the argument.
fromMutableByteArray :: PrimMonad m
  => MutableByteArray (PrimState m)
  -> m (MutableBytes (PrimState m))
{-# inline fromMutableByteArray #-}
fromMutableByteArray mba = do
  sz <- PM.getSizeofMutableByteArray mba
  pure (MutableBytes mba 0 sz)

-- Internal. The returns the number of bytes that match the
-- predicate until the first non-match occurs. If all bytes
-- match the predicate, this will return the length originally
-- provided.
countWhile :: PrimMonad m
  => (Word8 -> m Bool)
  -> MutableBytes (PrimState m)
  -> m Int
{-# inline countWhile #-}
countWhile k (MutableBytes arr off0 len0) = go off0 len0 0 where
  go !off !len !n = if len > 0
    then (k =<< PM.readByteArray arr off) >>= \case
      True -> go (off + 1) (len - 1) (n + 1)
      False -> pure n
    else pure n
