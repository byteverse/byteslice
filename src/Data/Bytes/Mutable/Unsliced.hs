{-# language BangPatterns #-}

module Data.Bytes.Mutable.Unsliced
  ( thawByteString
  ) where

import GHC.Exts (Ptr(..),RealWorld)
import Data.Primitive (MutableByteArray)
import Data.Primitive.Addr (Addr(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as B
import qualified Data.Primitive.Addr as PM
import qualified Data.Primitive as PM

-- | Copy a bytestring into a freshly allocated mutable byte array.
thawByteString :: ByteString -> IO (MutableByteArray RealWorld)
thawByteString b = B.unsafeUseAsCStringLen b
  (\(ptr,len) -> do
    x <- PM.newByteArray len
    PM.copyAddrToByteArray x 0 (ptrToAddr ptr) len
    pure x
  )

ptrToAddr :: Ptr a -> Addr
ptrToAddr (Ptr x) = Addr x
