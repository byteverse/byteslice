{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

{- | This module treats 'Bytes' data as holding text encoded in ISO-8859-1. This
encoding can only encode codepoints strictly below @U+0100@, but this allows
each codepoint to be placed directly into a single byte. This range consists
of Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls, which includes
ASCII.

Strictly, ISO-8859-1 is not to be confused with ISO/IEC 8859-1 (which was the
default encoding for webpages before HTML5). ISO/IEC 8859-1 lacks encodings
for the C0 and C1 control characters.

With HTML5, the default encoding of webpages was changed to Windows-1252,
which is _not_ compatible with ISO-8859-1. Windows-1252 uses the C1 Control
range (@U+0080@ -- @U+009F@) mostly to encode a variety of printable
characters. For this encoding, see 'Data.Bytes.Text.Windows1252'.
-}
module Data.Bytes.Text.Latin1
  ( toString
  , fromString
  , decodeDecWord

    -- * Specialized Comparisons
  , equals1
  , equals2
  , equals3
  , equals4
  , equals5
  , equals6
  , equals7
  , equals8
  , equals9
  , equals10
  , equals11
  , equals12
  , equals13
  , equals14
  , equals15
  ) where

import Prelude hiding (length)

import Data.Bytes.Types (Bytes (..))
import Data.Char (chr, ord)
import Data.Primitive (ByteArray (ByteArray))
import Data.Word (Word8)
import GHC.Exts (Char (C#), Int (I#), Word (W#), Word#, int2Word#, ltWord#, or#)

import qualified Data.Bytes.Pure as Bytes
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

{- | Convert a 'String' consisting of only characters representable
by ISO-8859-1. These are encoded with ISO-8859-1. Any character
with a codepoint above @U+00FF@ is replaced by an unspecified byte.
-}
fromString :: String -> Bytes
fromString =
  Bytes.fromByteArray . Exts.fromList . map (fromIntegral @Int @Word8 . ord)

-- | Interpret a byte sequence as text encoded by ISO-8859-1.
toString :: Bytes -> String
{-# INLINE toString #-}
toString = Bytes.foldr (\w xs -> chr (fromIntegral @Word8 @Int w) : xs) []

-- TODO presumably also fromText and fromShortText

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a singleton whose element matches the character?
-}
equals1 :: Char -> Bytes -> Bool
{-# INLINE equals1 #-}
equals1 !c0 (Bytes arr off len) = case len of
  1 -> c0 == indexCharArray arr off
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a doubleton whose elements match the characters?
-}
equals2 :: Char -> Char -> Bytes -> Bool
equals2 !c0 !c1 (Bytes arr off len) = case len of
  2 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a tripleton whose elements match the characters?
-}
equals3 :: Char -> Char -> Char -> Bytes -> Bool
equals3 !c0 !c1 !c2 (Bytes arr off len) = case len of
  3 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a quadrupleton whose elements match the characters?
-}
equals4 :: Char -> Char -> Char -> Char -> Bytes -> Bool
equals4 !c0 !c1 !c2 !c3 (Bytes arr off len) = case len of
  4 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a quintupleton whose elements match the characters?
-}
equals5 :: Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals5 !c0 !c1 !c2 !c3 !c4 (Bytes arr off len) = case len of
  5 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a sextupleton whose elements match the characters?
-}
equals6 :: Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals6 !c0 !c1 !c2 !c3 !c4 !c5 (Bytes arr off len) = case len of
  6 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a septupleton whose elements match the characters?
-}
equals7 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals7 !c0 !c1 !c2 !c3 !c4 !c5 !c6 (Bytes arr off len) = case len of
  7 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
an octupleton whose elements match the characters?
-}
equals8 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals8 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 (Bytes arr off len) = case len of
  8 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 9-tuple whose elements match the characters?
-}
equals9 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals9 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 (Bytes arr off len) = case len of
  9 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 10-tuple whose elements match the characters?
-}
equals10 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals10 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 (Bytes arr off len) = case len of
  10 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 11-tuple whose elements match the characters?
-}
equals11 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals11 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 (Bytes arr off len) = case len of
  11 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
      && c10 == indexCharArray arr (off + 10)
  _ -> False

{- | Is the byte sequence, when interpreted as ISO-8859-1-encoded text,
a 12-tuple whose elements match the characters?
-}
equals12 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals12 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 !c11 (Bytes arr off len) = case len of
  12 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
      && c10 == indexCharArray arr (off + 10)
      && c11 == indexCharArray arr (off + 11)
  _ -> False

equals13 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals13 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 !c11 !c12 (Bytes arr off len) = case len of
  13 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
      && c10 == indexCharArray arr (off + 10)
      && c11 == indexCharArray arr (off + 11)
      && c12 == indexCharArray arr (off + 12)
  _ -> False

equals14 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals14 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 !c11 !c12 !c13 (Bytes arr off len) = case len of
  14 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
      && c10 == indexCharArray arr (off + 10)
      && c11 == indexCharArray arr (off + 11)
      && c12 == indexCharArray arr (off + 12)
      && c13 == indexCharArray arr (off + 13)
  _ -> False

equals15 :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char -> Bytes -> Bool
equals15 !c0 !c1 !c2 !c3 !c4 !c5 !c6 !c7 !c8 !c9 !c10 !c11 !c12 !c13 !c14 (Bytes arr off len) = case len of
  15 ->
    c0 == indexCharArray arr off
      && c1 == indexCharArray arr (off + 1)
      && c2 == indexCharArray arr (off + 2)
      && c3 == indexCharArray arr (off + 3)
      && c4 == indexCharArray arr (off + 4)
      && c5 == indexCharArray arr (off + 5)
      && c6 == indexCharArray arr (off + 6)
      && c7 == indexCharArray arr (off + 7)
      && c8 == indexCharArray arr (off + 8)
      && c9 == indexCharArray arr (off + 9)
      && c10 == indexCharArray arr (off + 10)
      && c11 == indexCharArray arr (off + 11)
      && c12 == indexCharArray arr (off + 12)
      && c13 == indexCharArray arr (off + 13)
      && c14 == indexCharArray arr (off + 14)
  _ -> False

indexCharArray :: ByteArray -> Int -> Char
indexCharArray (ByteArray arr) (I# off) = C# (Exts.indexCharArray# arr off)

{- | Decode machine-sized word from decimal representation. Returns
Nothing on overflow. Allows any number of leading zeros. Trailing
non-digit bytes cause Nothing to be returned.
-}
decodeDecWord :: Bytes -> Maybe Word
{-# INLINE decodeDecWord #-}
decodeDecWord !b = case decWordStart b of
  (# (# #) | #) -> Nothing
  (# | w #) -> Just (W# w)

decWordStart ::
  Bytes -> -- Chunk
  (# (# #) | Word# #)
{-# NOINLINE decWordStart #-}
decWordStart !chunk0 =
  if length chunk0 > 0
    then
      let !w =
            fromIntegral @Word8 @Word
              (PM.indexByteArray (array chunk0) (offset chunk0))
              - 48
       in if w < 10
            then decWordMore w (Bytes.unsafeDrop 1 chunk0)
            else (# (# #) | #)
    else (# (# #) | #)
 where
  decWordMore ::
    Word -> -- Accumulator
    Bytes -> -- Chunk
    (# (# #) | Word# #)
  decWordMore !acc !chunk =
    let len = length chunk
     in case len of
          0 -> (# | unW (fromIntegral acc) #)
          _ ->
            let !w =
                  fromIntegral @Word8 @Word
                    (PM.indexByteArray (array chunk) (offset chunk))
                    - 48
             in if w < 10
                  then
                    let (overflow, acc') = unsignedPushBase10 acc w
                     in if overflow
                          then (# (# #) | #)
                          else decWordMore acc' (Bytes.unsafeDrop 1 chunk)
                  else (# (# #) | #)

unsignedPushBase10 :: Word -> Word -> (Bool, Word)
{-# INLINE unsignedPushBase10 #-}
unsignedPushBase10 (W# a) (W# b) =
  let !(# ca, r0 #) = Exts.timesWord2# a 10##
      !r1 = Exts.plusWord# r0 b
      !cb = int2Word# (ltWord# r1 r0)
      !c = ca `or#` cb
   in (case c of 0## -> False; _ -> True, W# r1)

unW :: Word -> Word#
{-# INLINE unW #-}
unW (W# w) = w
