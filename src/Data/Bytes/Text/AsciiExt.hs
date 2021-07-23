{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | This module contains functions which operate on supersets of 'Bytes' containing ASCII-encoded text.
-- That is, none of the functions here inspect bytes with a value greater than 127, and do not fail due to the presence of such bytes.

-- For functions that can fail for bytes outside the ASCII range, see 'Data.Bytes.Ascii'.
-- For functions that can inspect bytes outside ASCII, see any of the modules for ASCII-compatible encodings (e.g. 'Data.Bytes.Utf8', 'Data.Bytes.Latin1', and so on).
module Data.Bytes.Text.AsciiExt
  ( -- * Line-Oriented IO
    hFoldLines
  , hForLines_
  -- ** Standard Handles
  , forLines_
  , foldLines
  ) where

import Data.Bytes (Bytes)
import System.IO (Handle, hIsEOF, stdin)

import qualified Data.Bytes as Bytes
import qualified Data.ByteString.Char8 as BC8

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
-- dectection algorithm. As of writing (bytestring v0.11.1.0), lines are
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
-- dectection algorithm. As of writing (bytestring v0.11.1.0), lines are
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
