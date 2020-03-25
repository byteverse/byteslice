{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

module Data.Bytes.IO
  ( hGet
  ) where

import Data.Primitive (MutableByteArray)
import Data.Word (Word8)
import Data.Bytes.Types (Bytes(Bytes))
import System.IO (Handle)
import Foreign.Ptr (Ptr)
import GHC.IO (IO(IO))
import qualified System.IO as IO
import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM

-- | Read 'Bytes' directly from the specified 'Handle'. The resulting
-- 'Bytes' are pinned. This is implemented with 'hGetBuf'.
hGet :: Handle -> Int -> IO Bytes
hGet h i = createPinnedAndTrim i (\p -> IO.hGetBuf h p i)

-- Only used internally.
createPinnedAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO Bytes
{-# inline createPinnedAndTrim #-}
createPinnedAndTrim maxSz f = do
  arr@(PM.MutableByteArray arr#) <- PM.newPinnedByteArray maxSz
  sz <- f (PM.mutableByteArrayContents arr)
  touchMutableByteArrayIO arr
  PM.shrinkMutablePrimArray (PM.MutablePrimArray @Exts.RealWorld @Word8 arr#) sz
  r <- PM.unsafeFreezeByteArray arr
  pure (Bytes r 0 sz)

touchMutableByteArrayIO :: MutableByteArray s -> IO ()
touchMutableByteArrayIO (PM.MutableByteArray x) =
  IO (\s -> (# Exts.touch# x s, () #))

