{-# language CPP #-}
{-# language BangPatterns #-}

-- | Convert 'Bytes' to and from 'Text' and 'ShortText'.
module Data.Bytes.Text.Utf8
  ( fromShortText
  , toShortText
#if MIN_VERSION_text(2,0,0)
  , fromText
#endif
  ) where

import Data.Bytes.Types (Bytes(Bytes))
import Data.Text.Short (ShortText)
import Data.Text (Text)
import Data.Primitive (ByteArray(ByteArray))

import qualified Data.Text.Short as TS
import qualified Data.Bytes as Bytes
import qualified Data.Text.Internal as I
import qualified Data.Text.Array as A

-- | Encode 'ShortText' using UTF-8. Since 'ShortText' is backed by a UTF-8
-- byte sequence, this does not perform a copy.
fromShortText :: ShortText -> Bytes
{-# inline fromShortText #-}
fromShortText = Bytes.fromShortByteString . TS.toShortByteString

-- | Attempt to interpret the byte sequence as UTF-8 encoded text. Returns
-- 'Nothing' if the bytes are not UTF-8 encoded text.
toShortText :: Bytes -> Maybe ShortText
{-# inline toShortText #-}
toShortText !b = TS.fromShortByteString (Bytes.toShortByteString b)

#if MIN_VERSION_text(2,0,0)
-- | Encode 'Text' using 'UTF-8'. Only available when building with
-- @text-2.0@ and newer. Since 'Text' is backed by a UTF-8
-- byte sequence, this does not perform a copy.
fromText :: Text -> Bytes
{-# inline fromText #-}
fromText (I.Text (A.ByteArray b) off len) = Bytes (ByteArray b) off len
#endif
