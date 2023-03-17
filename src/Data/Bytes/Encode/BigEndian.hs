{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Data.Bytes.Encode.BigEndian
  ( int32
  , word32
  ) where

import Control.Monad.ST.Run (runByteArrayST)
import Data.Bits (unsafeShiftR)
import Data.Bytes.Types (Bytes)
import Data.Int (Int32)
import Data.Primitive (ByteArray)
import Data.Word (Word8,Word32)

import qualified Data.Bytes.Pure as Pure
import qualified Data.Primitive as PM

-- | Encode a 32-bit signed integer as 4 bytes.
int32 :: Int32 -> Bytes
{-# inline int32 #-}
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
