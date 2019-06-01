{-# language BangPatterns #-}
{-# language MagicHash #-}

module Data.Bytes.Unsliced
  ( toByteString
  ) where

import Control.Monad.ST (runST)
import Data.ByteString (ByteString)
import Data.Primitive (ByteArray)
import GHC.Exts (Ptr(..),MutableByteArray#,Addr#,unsafeCoerce#)

import qualified Data.ByteString.Unsafe as BU
import qualified Data.Primitive as PM
import qualified Data.Primitive.Addr as PMA
import qualified Data.ByteString.Internal as BSI
import qualified GHC.ForeignPtr as FP

toByteString :: ByteArray -> ByteString
toByteString a = do
  BSI.PS (FP.ForeignPtr (unPtr (PM.byteArrayContents y)) (FP.PlainPtr (veryUnsafeThaw y))) 0 sz
  where
  !sz = PM.sizeofByteArray a
  y = if PM.isByteArrayPinned a
    then a
    else runST $ do
      x <- PM.newPinnedByteArray sz
      PM.copyByteArray x 0 a 0 sz
      PM.unsafeFreezeByteArray x

fromByteString :: ByteString -> IO ByteArray
fromByteString b = BU.unsafeUseAsCStringLen b $ \(ptr,sz) -> do
  m <- PM.newByteArray sz
  PMA.copyAddrToByteArray m 0 (PMA.Addr (unPtr ptr)) sz
  PM.unsafeFreezeByteArray m

veryUnsafeThaw :: ByteArray -> MutableByteArray# s
veryUnsafeThaw (PM.ByteArray x) = unsafeCoerce# x

unPtr :: Ptr a -> Addr#
unPtr (Ptr x) = x

