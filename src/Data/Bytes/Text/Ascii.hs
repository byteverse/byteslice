{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

{- | This module treats 'Bytes' data as holding ASCII text. Providing bytes
outside the ASCII range (@U+0000@ -- @U+007F@) may cause a failure or
unspecified results, but such bytes will never be inspected.

For functions that can operate on ASCII-compatible encodings, see
'Data.Bytes.Text.AsciiExt'.
-}
module Data.Bytes.Text.Ascii
  ( fromString
  , decodeDecWord
  , equalsCStringCaseInsensitive
  , toShortText
  , toShortTextU
#if MIN_VERSION_text(2,0,0)
  , toText
#endif
  ) where

import Data.Bits ((.&.))
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Bytes.Text.Latin1 (decodeDecWord)
import Data.Bytes.Types (Bytes (Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Text (Text)
import Data.Text.Short (ShortText)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr, castPtr, plusPtr)

import qualified Data.Bytes.Pure as Bytes
import qualified Data.Primitive as PM
import qualified Data.Primitive.Ptr as PM
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as I
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts

{- | Convert a 'String' consisting of only characters in the ASCII block
to a byte sequence. Any character with a codepoint above @U+007F@ is
replaced by @U+0000@.
-}
fromString :: String -> Bytes
fromString =
  Bytes.fromByteArray
    . Exts.fromList
    . map (\c -> let i = ord c in if i < 128 then fromIntegral @Int @Word8 i else 0)

-- TODO presumably also fromText and fromShortText

toShortText :: Bytes -> Maybe ShortText
{-# INLINE toShortText #-}
toShortText !b = case Bytes.foldr (\w acc -> w < 128 && acc) True b of
  True -> Just (TS.fromShortByteStringUnsafe (Bytes.toShortByteString b))
  False -> Nothing

toShortTextU :: ByteArray -> Maybe ShortText
{-# INLINE toShortTextU #-}
toShortTextU !b = case Bytes.foldr (\w acc -> w < 128 && acc) True (Bytes.fromByteArray b) of
  True -> Just (TS.fromShortByteStringUnsafe (case b of PM.ByteArray x -> SBS x))
  False -> Nothing

#if MIN_VERSION_text(2,0,0)
-- | Interpret byte sequence as ASCII codepoints.
-- Only available when building with @text-2.0@ and newer.
-- Returns 'Nothing' if any of the bytes are outside of the
-- range @0x00-0x7F@
toText :: Bytes -> Maybe Text
{-# inline toText #-}
toText !b@(Bytes (PM.ByteArray arr) off len) = case Bytes.foldr (\w acc -> w < 128 && acc) True b of
  True -> Just (I.Text (A.ByteArray arr) off len)
  False -> Nothing
#endif

{- | Is the byte sequence equal to the @NUL@-terminated C String?
The C string must be a constant.
-}
equalsCStringCaseInsensitive :: CString -> Bytes -> Bool
{-# INLINE equalsCStringCaseInsensitive #-}
equalsCStringCaseInsensitive !ptr0 (Bytes arr off0 len0) = go (castPtr ptr0 :: Ptr Word8) off0 len0
 where
  go !ptr !off !len = case len of
    0 -> PM.indexOffPtr ptr 0 == (0 :: Word8)
    _ -> case PM.indexOffPtr ptr 0 of
      0 -> False
      c ->
        (c .&. 0b1101_1111) == (PM.indexByteArray arr off .&. 0b1101_1111)
          && go (plusPtr ptr 1) (off + 1) (len - 1)
