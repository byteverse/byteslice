{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
  , toText
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
import GHC.Exts (Int#,Word#,ByteArray#,(+#),(<#))
import GHC.Int (Int(I#))

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

-- | Interpret byte sequence as ASCII codepoints.
-- Returns 'Nothing' if any of the bytes are outside of the
-- range @0x00-0x7F@
--
-- This does not work on 32-bit architectures. I am not certain if it
-- even compiles on those systems.
toText :: Bytes -> Maybe Text
toText (Bytes (PM.ByteArray arr) off@(I# off# ) len@(I# len# )) =
  let !r0 = validateAscii# arr off# len# (off# +# len# )
      !r1 = Exts.and# r0 0b1000_0000_1000_0000_1000_0000_1000_0000_1000_0000_1000_0000_1000_0000_1000_0000##
   in case r1 of
        0## -> Just (I.Text (A.ByteArray arr) off len)
        _ -> Nothing

-- returns 0 to mean that the input slice was all ascii text
-- any other number means that a non-ascii byte was encountered
-- Precondition: len and end must agree with one another.
validateAscii# :: ByteArray# -> Int# -> Int# -> Int# -> Word#
{-# noinline validateAscii# #-}
validateAscii# arr off len end
  -- This length check at the beginning is not just a performance optimization.
  -- If the length of the slice is less than 8, the calculations of "middle start"
  -- and "middle end" in the otherwise clause are out of bounds.
  | 1# <- len <# 20# = validateRangeSlowly# 0## off end arr
  | otherwise =
      let !middleStartSwar = Exts.uncheckedIShiftRL# (off +# 7#) 3#
          !middleEndSwar = Exts.uncheckedIShiftRL# end 3#
          !w0 = validateRangeSwar# 0## middleStartSwar middleEndSwar arr
          !w1 = validateRangeSlowly# w0 off (Exts.uncheckedIShiftL# middleStartSwar 3# ) arr
          !w2 = validateRangeSlowly# w1 (Exts.uncheckedIShiftL# middleEndSwar 3# ) end arr
       in w2

-- Here, the offset and the end refer to 64-bit word units, not byte units
-- like they do in validateRangeSlowly.
validateRangeSwar# :: Word# -> Int# -> Int# -> ByteArray# -> Word#
{-# inline validateRangeSwar# #-}
validateRangeSwar# acc0 off0 end arr = go off0 acc0
  where
  go :: Int# -> Word# -> Word#
  go off acc = case off <# end of
    1# -> go (off +# 1#) (Exts.or# (Exts.indexWordArray# arr off) acc)
    _ -> acc

-- Accepts a start and end position. The end position is exclusive.
validateRangeSlowly# :: Word# -> Int# -> Int# -> ByteArray# -> Word#
{-# inline validateRangeSlowly# #-}
validateRangeSlowly# acc0 off0 end arr = go off0 acc0
  where
  go :: Int# -> Word# -> Word#
  go off acc = case off <# end of
    1# -> go (off +# 1#) (Exts.or# (Exts.word8ToWord# (Exts.indexWord8Array# arr off)) acc)
    _ -> acc

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
