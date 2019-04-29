{-# language BangPatterns #-}
{-# language MagicHash #-}

module Data.Bytes.Types
  ( Bytes(..)
  , MutableBytes(..)
  ) where

import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (Int(I#),unsafeCoerce#,sameMutableByteArray#)
import GHC.Exts (isTrue#,compareByteArrays#)

-- | A slice of a 'ByteArray'.
data Bytes = Bytes
  {-# UNPACK #-} !ByteArray -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

-- | A slice of a 'MutableByteArray'.
data MutableBytes s = MutableBytes
  {-# UNPACK #-} !(MutableByteArray s) -- payload
  {-# UNPACK #-} !Int -- offset
  {-# UNPACK #-} !Int -- length

instance Eq Bytes where
  {-# INLINE (==) #-}
  Bytes arr1 off1 len1 == Bytes arr2 off2 len2
    | len1 /= len2 = False
    | sameByteArray arr1 arr2 && off1 == off2 = True
    | otherwise = compareByteArrays arr1 off1 arr2 off2 len1 == EQ

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (compareByteArrays# ba1# off1# ba2# off2# n#)) 0

sameByteArray :: ByteArray -> ByteArray -> Bool
{-# INLINE sameByteArray #-}
sameByteArray (ByteArray ba1#) (ByteArray ba2#) =
  isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))
