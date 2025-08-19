{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Convert 'Bytes' to and from 'Text' and 'ShortText'.
module Data.Bytes.Text.Utf8
  ( fromShortText
  , toShortText
  , fromText
  , toText
  ) where

import Data.Bytes.Types (Bytes (Bytes))
import Data.Primitive (ByteArray (ByteArray))
import Data.Text (Text)
import Data.Text.Short (ShortText)

import qualified Data.Bytes as Bytes
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as I
import qualified Data.Text.Short as TS

import qualified Data.Text.Internal.Validate

{- | Encode 'ShortText' using UTF-8. Since 'ShortText' is backed by a UTF-8
byte sequence, this does not perform a copy.
-}
fromShortText :: ShortText -> Bytes
{-# INLINE fromShortText #-}
fromShortText = Bytes.fromShortByteString . TS.toShortByteString

{- | Attempt to interpret the byte sequence as UTF-8 encoded text. Returns
'Nothing' if the bytes are not UTF-8 encoded text.
-}
toShortText :: Bytes -> Maybe ShortText
{-# INLINE toShortText #-}
toShortText !b = TS.fromShortByteString (Bytes.toShortByteString b)

-- | Encode 'Text' using @UTF-8@. Only available when building with
-- @text-2.0@ and newer. Since 'Text' is backed by a UTF-8
-- byte sequence, this does not perform a copy.
fromText :: Text -> Bytes
{-# inline fromText #-}
fromText (I.Text (A.ByteArray b) off len) = Bytes (ByteArray b) off len

-- | Attempt to interpret byte sequence as @UTF-8@ encoded 'Text'.
-- Only available when building with @text-2.1@ and newer. Since
-- 'Text' is backed by a UTF-8 byte sequence, this does not perform a
-- copy.
toText :: Bytes -> Maybe Text
{-# inline toText #-}
toText (Bytes b@(ByteArray b0) off len) =
  if Data.Text.Internal.Validate.isValidUtf8ByteArray b off len
    then Just (I.Text (A.ByteArray b0) off len)
    else Nothing
