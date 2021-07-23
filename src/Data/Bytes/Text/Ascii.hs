{-# LANGUAGE TypeApplications #-}

-- | This module treats 'Bytes' data as holding ASCII text. Providing bytes
-- outside the ASCII range (@U+0000@ -- @U+007F@) may cause a failure or
-- unspecified results, but such bytes will never be inspected.
--
-- For functions that can operate on ASCII-compatible encodings, see
-- 'Data.Bytes.Text.AsciiExt'.
module Data.Bytes.Text.Ascii
  ( fromString
  ) where

import Data.Bytes.Types (Bytes)
import Data.Char (ord)
import Data.Word (Word8)

import qualified Data.Bytes.Pure as Bytes
import qualified GHC.Exts as Exts


-- | Convert a 'String' consisting of only characters in the ASCII block
-- to a byte sequence. Any character with a codepoint above @U+007F@ is
-- replaced by @U+0000@.
fromString :: String -> Bytes
fromString = Bytes.fromByteArray
  . Exts.fromList
  . map (\c -> let i = ord c in if i < 128 then fromIntegral @Int @Word8 i else 0)

-- TODO presumably also fromText and fromShortText
