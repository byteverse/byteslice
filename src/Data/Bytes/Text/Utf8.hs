-- | Placeholder module in case there is demand for treating 'Bytes' as
-- UTF8-encoded text
module Data.Bytes.Text.Utf8
  ( fromShortText
  ) where

import Data.Bytes (Bytes)
import Data.Text.Short (ShortText)

import qualified Data.Text.Short as TS
import qualified Data.Bytes as Bytes

-- | Encode 'ShortText' using UTF-8. Since 'ShortText' is backed by a UTF-8
-- byte sequence, this does not perform a copy.
fromShortText :: ShortText -> Bytes
{-# inline fromShortText #-}
fromShortText = Bytes.fromShortByteString . TS.toShortByteString
