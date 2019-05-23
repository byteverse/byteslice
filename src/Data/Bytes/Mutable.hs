module Data.Bytes.Mutable
  ( thawByteString
  , fromUnsliced
  ) where

import Control.Monad ((<=<))
import Control.Monad.Primitive (PrimState,PrimMonad)
import Data.Bytes.Types (MutableBytes(..))
import Data.ByteString (ByteString)
import GHC.Exts (RealWorld)
import Data.Primitive (MutableByteArray)

import qualified Data.Primitive as PM
import qualified Data.Bytes.Mutable.Unsliced as U

thawByteString :: ByteString -> IO (MutableBytes RealWorld)
thawByteString = fromUnsliced <=< U.thawByteString

fromUnsliced :: PrimMonad m
  => MutableByteArray (PrimState m)
  -> m (MutableBytes (PrimState m))
fromUnsliced a = do
  sz <- PM.getSizeofMutableByteArray a
  pure (MutableBytes a 0 sz)
