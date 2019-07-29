{-# language BangPatterns #-}
{-# language TypeApplications #-}

module Data.Bytes
  ( Bytes
  , null
  , length
  , takeWhile
  , toByteArray
  , toByteArrayClone
    -- * Conversion
  , fromAsciiString
  , fromByteArray
  ) where

import Prelude hiding (length,takeWhile,null)

import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Data.Char (ord)
import Control.Monad.ST.Run (runByteArrayST)

import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

-- | Is the byte sequence empty?
null :: Bytes -> Bool
null (Bytes _ _ len) = len == 0

-- | The length of a slice of bytes.
length :: Bytes -> Int
length (Bytes _ _ len) = len

takeWhile :: (Word8 -> Bool) -> Bytes -> Bytes
{-# inline takeWhile #-}
takeWhile k b = unsafeTake (countWhile k b) b

unsafeTake :: Int -> Bytes -> Bytes
{-# inline unsafeTake #-}
unsafeTake n (Bytes arr off len) =
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

fromByteArray :: ByteArray -> Bytes
fromByteArray b = Bytes b 0 (PM.sizeofByteArray b)

