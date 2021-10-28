{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | This module contains functions which operate on supersets of 'Bytes' containing ASCII-encoded text.
-- That is, none of the functions here inspect bytes with a value greater than 127, and do not fail due to the presence of such bytes.

-- For functions that can fail for bytes outside the ASCII range, see
-- 'Data.Bytes.Ascii'. For functions that can inspect bytes outside ASCII, see
-- any of the modules for ASCII-compatible encodings (e.g. 'Data.Bytes.Utf8',
-- 'Data.Bytes.Latin1', and so on).
module Data.Bytes.Text.AsciiExt
  ( -- * Line-Oriented IO
    hFoldLines
  , hForLines_
  -- ** Standard Handles
  , forLines_
  , foldLines
  -- * Text Manipulation
  , toLowerU
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(..))
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import System.IO (Handle, hIsEOF, stdin)

import qualified Data.Bytes.Pure as Bytes
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Primitive as PM

-- | `hForLines_` over `stdin`
forLines_ :: (Bytes -> IO a) -> IO ()
{-# INLINEABLE forLines_ #-}
forLines_ = hForLines_ stdin

-- | `hFoldLines` over `stdin`
foldLines :: a -> (a -> Bytes -> IO a) -> IO a
{-# INLINEABLE foldLines #-}
foldLines = hFoldLines stdin

-- | Perform an action on each line of the input, discarding results.
-- To maintain a running state, see 'hFoldLines'.
--
-- Lines are extracted with with 'BC8.hGetLine', which does not document its
-- detection algorithm. As of writing (bytestring v0.11.1.0), lines are
-- delimited by a single @\n@ character (UNIX-style, as all things should be).
hForLines_ :: Handle -> (Bytes -> IO a) -> IO ()
hForLines_ h body = loop
  where
  loop = hIsEOF h >>= \case
    False -> do
      line <- Bytes.fromByteString <$> BC8.hGetLine h
      _ <- body line
      loop
    True -> pure ()

-- | Perform an action on each line of the input, threading state through the computation.
-- If you do not need to keep a state, see `hForLines_`.
--
-- Lines are extracted with with 'BC8.hGetLine', which does not document its
-- detection algorithm. As of writing (bytestring v0.11.1.0), lines are
-- delimited by a single @\n@ character (UNIX-style, as all things should be).
hFoldLines :: Handle -> a -> (a -> Bytes -> IO a) -> IO a
hFoldLines h z body = loop z
  where
  loop !x = hIsEOF h >>= \case
    False -> do
      line <- Bytes.fromByteString <$> BC8.hGetLine h
      x' <- body x line
      loop x'
    True -> pure x

-- | /O(n)/ Convert ASCII letters to lowercase. This adds @0x20@ to bytes in the
-- range @[0x41,0x5A]@ (@A-Z@ ⇒ @a-z@) and leaves all other bytes alone.
-- Unconditionally copies the bytes.
toLowerU :: Bytes -> ByteArray
toLowerU (Bytes src off0 len0) =
  runByteArrayST action
  where
  action :: forall s. ST s ByteArray
  action = do
    dst <- PM.newByteArray len0
    let go !off !ix !len = if len == 0
          then pure ()
          else do
            let w = PM.indexByteArray src off :: Word8
                w' = if w >= 0x41 && w <= 0x5A
                  then w + 32
                  else w
            PM.writeByteArray dst ix w'
            go (off + 1) (ix + 1) (len - 1)
    go off0 0 len0
    PM.unsafeFreezeByteArray dst
