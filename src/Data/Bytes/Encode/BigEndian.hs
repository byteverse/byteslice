{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Data.Bytes.Encode.BigEndian
  ( word16
  , word32
  , word64
  , int16
  , int32
  , int64
  ) where

import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (unsafeShiftR)
import Data.Bytes.Types (Bytes)
import Data.Int (Int16, Int32, Int64)
import Data.Primitive (ByteArray)
import Data.Word (Word16, Word32, Word64, Word8)

import qualified Data.Bytes.Pure as Pure
import qualified Data.Primitive as PM

-- | Encode a 32-bit signed integer as 4 bytes.
int32 :: Int32 -> Bytes
{-# INLINE int32 #-}
int32 = word32 . fromIntegral @Int32 @Word32

-- | Encode a 32-bit unsigned integer as 4 bytes.
word32 :: Word32 -> Bytes
word32 !w = Pure.fromByteArray (word32U w)

word32U :: Word32 -> ByteArray
word32U !w = runByteArrayST $ do
  arr <- PM.newByteArray 4
  PM.writeByteArray arr 0 (fromIntegral @Word32 @Word8 (unsafeShiftR w 24))
  PM.writeByteArray arr 1 (fromIntegral @Word32 @Word8 (unsafeShiftR w 16))
  PM.writeByteArray arr 2 (fromIntegral @Word32 @Word8 (unsafeShiftR w 8))
  PM.writeByteArray arr 3 (fromIntegral @Word32 @Word8 w)
  PM.unsafeFreezeByteArray arr

-- | Encode a 16-bit signed integer as 4 bytes.
int16 :: Int16 -> Bytes
{-# INLINE int16 #-}
int16 = word16 . fromIntegral @Int16 @Word16

-- | Encode a 16-bit unsigned integer as 4 bytes.
word16 :: Word16 -> Bytes
word16 !w = Pure.fromByteArray (word16U w)

word16U :: Word16 -> ByteArray
word16U !w = runByteArrayST $ do
  arr <- PM.newByteArray 2
  PM.writeByteArray arr 0 (fromIntegral @Word16 @Word8 (unsafeShiftR w 8))
  PM.writeByteArray arr 1 (fromIntegral @Word16 @Word8 w)
  PM.unsafeFreezeByteArray arr

-- | Encode a 16-bit signed integer as 4 bytes.
int64 :: Int64 -> Bytes
{-# INLINE int64 #-}
int64 = word64 . fromIntegral @Int64 @Word64

-- | Encode a 16-bit unsigned integer as 4 bytes.
word64 :: Word64 -> Bytes
word64 !w = Pure.fromByteArray (word64U w)

word64U :: Word64 -> ByteArray
word64U !w = runByteArrayST $ do
  arr <- PM.newByteArray 8
  PM.writeByteArray arr 0 (fromIntegral @Word64 @Word8 (unsafeShiftR w 56))
  PM.writeByteArray arr 1 (fromIntegral @Word64 @Word8 (unsafeShiftR w 48))
  PM.writeByteArray arr 2 (fromIntegral @Word64 @Word8 (unsafeShiftR w 40))
  PM.writeByteArray arr 3 (fromIntegral @Word64 @Word8 (unsafeShiftR w 32))
  PM.writeByteArray arr 4 (fromIntegral @Word64 @Word8 (unsafeShiftR w 24))
  PM.writeByteArray arr 5 (fromIntegral @Word64 @Word8 (unsafeShiftR w 16))
  PM.writeByteArray arr 6 (fromIntegral @Word64 @Word8 (unsafeShiftR w 8))
  PM.writeByteArray arr 7 (fromIntegral @Word64 @Word8 w)
  PM.unsafeFreezeByteArray arr
