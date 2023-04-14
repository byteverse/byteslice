{-# language BangPatterns #-}
{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

-- This needs to be in its own module to prevent a cyclic dependency
-- between UnliftedBytes and Data.Bytes.Types
module Data.Bytes.Internal
  ( Bytes(..)
  ) where

import Control.Monad.ST (runST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Primitive (ByteArray(..))
import Data.Word (Word8)
import GHC.Exts (Int(I#),unsafeCoerce#,sameMutableByteArray#)
import GHC.Exts (isTrue#,compareByteArrays#,IsList(..))
import Data.Bytes.Internal.Show (showsSlice)

import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Primitive as PM

-- | A slice of a 'ByteArray'.
data Bytes = Bytes
  { array :: {-# UNPACK #-} !ByteArray
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }

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
  showsPrec _ (Bytes arr off len) s = showsSlice arr off len s

instance Eq Bytes where
  Bytes arr1 off1 len1 == Bytes arr2 off2 len2
    | len1 /= len2 = False
    | sameByteArray arr1 arr2 && off1 == off2 = True
    | otherwise = compareByteArrays arr1 off1 arr2 off2 len1 == EQ

instance Ord Bytes where
  compare (Bytes arr1 off1 len1) (Bytes arr2 off2 len2)
    | sameByteArray arr1 arr2 && off1 == off2 && len1 == len2 = EQ
    | otherwise = compareByteArrays arr1 off1 arr2 off2 (min len1 len2) <> compare len1 len2

instance Semigroup Bytes where
  -- TODO: Do the trick to move the data constructor to the outside
  -- of runST.
  Bytes arrA offA lenA <> Bytes arrB offB lenB = runST $ do
    marr <- PM.newByteArray (lenA + lenB)
    PM.copyByteArray marr 0 arrA offA lenA
    PM.copyByteArray marr lenA arrB offB lenB
    r <- PM.unsafeFreezeByteArray marr
    pure (Bytes r 0 (lenA + lenB))

instance Monoid Bytes where
  mempty = Bytes mempty 0 0
  mconcat [] = mempty
  mconcat [x] = x
  mconcat bs = Bytes r 0 fullLen
    where
    !fullLen = L.foldl' (\acc (Bytes _ _ len) -> acc + len) 0 bs
    r = runByteArrayST $ do
      marr <- PM.newByteArray fullLen
      !_ <- F.foldlM
        (\ !currLen (Bytes arr off len) -> do
          PM.copyByteArray marr currLen arr off len
          pure (currLen + len)
        ) 0 bs
      PM.unsafeFreezeByteArray marr

compareByteArrays :: ByteArray -> Int -> ByteArray -> Int -> Int -> Ordering
{-# INLINE compareByteArrays #-}
compareByteArrays (ByteArray ba1#) (I# off1#) (ByteArray ba2#) (I# off2#) (I# n#) =
  compare (I# (compareByteArrays# ba1# off1# ba2# off2# n#)) 0

sameByteArray :: ByteArray -> ByteArray -> Bool
{-# INLINE sameByteArray #-}
sameByteArray (ByteArray ba1#) (ByteArray ba2#) =
  isTrue# (sameMutableByteArray# (unsafeCoerce# ba1#) (unsafeCoerce# ba2#))

