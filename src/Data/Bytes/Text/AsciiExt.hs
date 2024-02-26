{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- For functions that can fail for bytes outside the ASCII range, see
-- 'Data.Bytes.Ascii'. For functions that can inspect bytes outside ASCII, see
-- any of the modules for ASCII-compatible encodings (e.g. 'Data.Bytes.Utf8',
-- 'Data.Bytes.Latin1', and so on).

{- | This module contains functions which operate on supersets of 'Bytes' containing ASCII-encoded text.
That is, none of the functions here inspect bytes with a value greater than 127, and do not fail due to the presence of such bytes.
-}
module Data.Bytes.Text.AsciiExt
  ( -- * Line-Oriented IO
    hFoldLines
  , hForLines_

    -- ** Standard Handles
  , forLines_
  , foldLines

    -- * Predicates
  , anyEq

    -- * Filtering
  , takeWhileNotEq
  , dropWhileNotEq
  , takeWhileEndNotEq
  , dropWhileEndEq

    -- * Splitting
    -- ** Fixed from Beginning
  , split1
  , splitTetragram1
  , split2
  , split3
  , split4

    -- * Text Manipulation
  , toLowerU
  ) where

import Control.Monad.ST (ST)
import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes (..))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import System.IO (Handle, hIsEOF, stdin)

import qualified Data.ByteString.Char8 as BC8
import qualified Data.Bytes.Pure as Bytes
import qualified Data.Bytes.Byte as Byte
import qualified Data.Primitive as PM

-- | `hForLines_` over `stdin`
forLines_ :: (Bytes -> IO a) -> IO ()
{-# INLINEABLE forLines_ #-}
forLines_ = hForLines_ stdin

-- | `hFoldLines` over `stdin`
foldLines :: a -> (a -> Bytes -> IO a) -> IO a
{-# INLINEABLE foldLines #-}
foldLines = hFoldLines stdin

{- | Perform an action on each line of the input, discarding results.
To maintain a running state, see 'hFoldLines'.

Lines are extracted with with 'BC8.hGetLine', which does not document its
detection algorithm. As of writing (bytestring v0.11.1.0), lines are
delimited by a single @\n@ character (UNIX-style, as all things should be).
-}
hForLines_ :: Handle -> (Bytes -> IO a) -> IO ()
hForLines_ h body = loop
 where
  loop =
    hIsEOF h >>= \case
      False -> do
        line <- Bytes.fromByteString <$> BC8.hGetLine h
        _ <- body line
        loop
      True -> pure ()

{- | Perform an action on each line of the input, threading state through the computation.
If you do not need to keep a state, see `hForLines_`.

Lines are extracted with with 'BC8.hGetLine', which does not document its
detection algorithm. As of writing (bytestring v0.11.1.0), lines are
delimited by a single @\n@ character (UNIX-style, as all things should be).
-}
hFoldLines :: Handle -> a -> (a -> Bytes -> IO a) -> IO a
hFoldLines h z body = loop z
 where
  loop !x =
    hIsEOF h >>= \case
      False -> do
        line <- Bytes.fromByteString <$> BC8.hGetLine h
        x' <- body x line
        loop x'
      True -> pure x

{- | /O(n)/ Convert ASCII letters to lowercase. This adds @0x20@ to bytes in the
range @[0x41,0x5A]@ (@A-Z@ ⇒ @a-z@) and leaves all other bytes alone.
Unconditionally copies the bytes.
-}
toLowerU :: Bytes -> ByteArray
toLowerU (Bytes src off0 len0) =
  runByteArrayST action
 where
  action :: forall s. ST s ByteArray
  action = do
    dst <- PM.newByteArray len0
    let go !off !ix !len =
          if len == 0
            then pure ()
            else do
              let w = PM.indexByteArray src off :: Word8
                  w' =
                    if w >= 0x41 && w <= 0x5A
                      then w + 32
                      else w
              PM.writeByteArray dst ix w'
              go (off + 1) (ix + 1) (len - 1)
    go off0 0 len0
    PM.unsafeFreezeByteArray dst

-- | Throws an exception the 'Char' argument is non-ascii.
split1 :: Char -> Bytes -> Maybe (Bytes, Bytes)
{-# INLINE split1 #-}
split1 !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.split1: argument not in ASCII range"
  | otherwise = Byte.split1 (c2w c) b

-- | Throws an exception the 'Char' argument is non-ascii.
split2 :: Char -> Bytes -> Maybe (Bytes, Bytes, Bytes)
{-# INLINE split2 #-}
split2 !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.split2: argument not in ASCII range"
  | otherwise = Byte.split2 (c2w c) b

-- | Throws an exception the 'Char' argument is non-ascii.
split3 :: Char -> Bytes -> Maybe (Bytes, Bytes, Bytes, Bytes)
{-# INLINE split3 #-}
split3 !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.split3: argument not in ASCII range"
  | otherwise = Byte.split3 (c2w c) b

-- | Throws an exception the 'Char' argument is non-ascii.
split4 :: Char -> Bytes -> Maybe (Bytes, Bytes, Bytes, Bytes, Bytes)
{-# INLINE split4 #-}
split4 !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.split4: argument not in ASCII range"
  | otherwise = Byte.split4 (c2w c) b

-- | Throws an exception if any of the 'Char' arguments are non-ascii.
splitTetragram1 :: Char -> Char -> Char -> Char -> Bytes -> Maybe (Bytes, Bytes)
{-# inline splitTetragram1 #-}
splitTetragram1 !c0 !c1 !c2 !c3 !b
  | c0 > '\DEL' || c1 > '\DEL' || c2 > '\DEL' || c3 > '\DEL' =
      errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.splitTetragram1: one of the characters is not in ASCII range"
  | otherwise = Bytes.splitTetragram1 (c2w c0) (c2w c1) (c2w c2) (c2w c3) b

c2w :: Char -> Word8
{-# inline c2w #-}
c2w !c = fromIntegral @Int @Word8 (ord c)

-- | Throws an exception the 'Char' argument is non-ascii.
dropWhileNotEq :: Char -> Bytes -> Bytes
dropWhileNotEq !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.dropWhileNotEq: argument not in ASCII range"
  | otherwise =
      let !w = c2w c
       in Bytes.unsafeDrop (Bytes.countWhile (/= w) b) b

-- | Throws an exception the 'Char' argument is non-ascii.
takeWhileNotEq :: Char -> Bytes -> Bytes
takeWhileNotEq !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.takeWhileNotEq: argument not in ASCII range"
  | otherwise =
      let !w = c2w c
       in Bytes.unsafeTake (Bytes.countWhile (/= w) b) b
  
-- | Throws an exception the 'Char' argument is non-ascii.
takeWhileEndNotEq :: Char -> Bytes -> Bytes
takeWhileEndNotEq !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.takeWhileEndNotEq: argument not in ASCII range"
  | otherwise =
      let !w = c2w c
          !n = Bytes.countWhileEnd (/=w) b
       in Bytes (array b) (offset b + Bytes.length b - n) n

-- | Throws an exception the 'Char' argument is non-ascii.
dropWhileEndEq :: Char -> Bytes -> Bytes
dropWhileEndEq !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.dropWhileEndEq: argument not in ASCII range"
  | otherwise =
      let !w = c2w c
          !n = Bytes.countWhileEnd (==w) b
       in Bytes.unsafeTake (Bytes.length b - n) b

-- | Throws an exception the 'Char' argument is non-ascii.
anyEq :: Char -> Bytes -> Bool
anyEq !c !b
  | c > '\DEL' = errorWithoutStackTrace "Data.Bytes.Text.AsciiExt.takeWhileNotEq: argument not in ASCII range"
  | otherwise =
      let !w = c2w c
       in Bytes.any (==w) b
