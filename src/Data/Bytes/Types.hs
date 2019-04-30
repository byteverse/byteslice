{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}

module Data.Bytes.Types
  ( Bytes(..)
  , MutableBytes(..)
  ) where

import Data.Primitive (ByteArray(..),MutableByteArray(..))
import Data.Bits ((.&.),unsafeShiftR)
import Data.Char (ord)
import Data.Word (Word8)
import GHC.Base (unsafeChr)
import GHC.Exts (Int(I#),unsafeCoerce#,sameMutableByteArray#)
import GHC.Exts (isTrue#,compareByteArrays#,IsList(..))

import qualified Data.List as L
import qualified Data.Primitive as PM

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

instance IsList Bytes where
  type Item Bytes = Word8
  fromListN n xs = Bytes (fromListN n xs) 0 n
  fromList xs = fromListN (L.length xs) xs
  toList (Bytes arr off len) = toListLoop off len arr

toListLoop :: Int -> Int -> ByteArray -> [Word8]
toListLoop !off !len !arr = if len > 0
  then PM.indexByteArray arr off : toListLoop (off + 1) (len - 1) arr
  else []

instance Show Bytes where
  showsPrec d (Bytes arr off len) s = if len == 0
    then showString "[]" s
    else showString "[0x"
       $ showHexDigits (PM.indexByteArray arr off)
       $ showLoop (off + 1) (len - 1) arr
       $ showChar ']'
       $ s

showLoop :: Int -> Int -> ByteArray -> String -> String
showLoop !ix !len !arr s = if len > 0
  then ',':'0':'x':showHexDigits (PM.indexByteArray arr ix) (showLoop (ix + 1) (len - 1) arr s)
  else s

showHexDigits :: Word8 -> String -> String
showHexDigits !w s = word4ToChar (unsafeShiftR w 4) : word4ToChar (0x0F .&. w) : s

word4ToChar :: Word8 -> Char
word4ToChar w = if w < 10
  then unsafeChr (ord '0' + fromIntegral w)
  else unsafeChr (ord 'a' + (fromIntegral w) - 10)

instance Eq Bytes where
  {-# INLINE (==) #-}
  Bytes arr1 off1 len1 == Bytes arr2 off2 len2
    | len1 /= len2 = False
    | sameByteArray arr1 arr2 && off1 == off2 = True
    | otherwise = compareByteArrays arr1 off1 arr2 off2 len1 == EQ

instance Ord Bytes where
  {-# INLINE compare #-}
  compare (Bytes arr1 off1 len1) (Bytes arr2 off2 len2)
    | sameByteArray arr1 arr2 && off1 == off2 && len1 == len2 = EQ
    | otherwise = compareByteArrays arr1 off1 arr2 off2 len1

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (compareByteArrays# ba1# off1# ba2# off2# n#)) 0

sameByteArray :: ByteArray -> ByteArray -> Bool
{-# INLINE sameByteArray #-}
sameByteArray (ByteArray ba1#) (ByteArray ba2#) =
  isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))
