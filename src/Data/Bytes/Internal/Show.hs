{-# LANGUAGE BangPatterns #-}

module Data.Bytes.Internal.Show
  ( showsSlice
  ) where

import Data.Bits (unsafeShiftR, (.&.))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Word (Word8)
import GHC.Base (unsafeChr)

import qualified Data.Primitive as PM

showsSlice :: ByteArray -> Int -> Int -> String -> String
showsSlice arr off len s =
  if len == 0
    then showString "[]" s
    else
      showString "[0x" $
        showHexDigitsWord8 (PM.indexByteArray arr off) $
          showHexLoop (off + 1) (len - 1) arr $
            showChar ']' $
              s

showHexLoop :: Int -> Int -> ByteArray -> String -> String
showHexLoop !ix !len !arr s =
  if len > 0
    then ',' : '0' : 'x' : showHexDigitsWord8 (PM.indexByteArray arr ix) (showHexLoop (ix + 1) (len - 1) arr s)
    else s

showHexDigitsWord8 :: Word8 -> String -> String
showHexDigitsWord8 !w s = word4ToChar (unsafeShiftR w 4) : word4ToChar (0x0F .&. w) : s

word4ToChar :: Word8 -> Char
word4ToChar w =
  if w < 10
    then unsafeChr (ord '0' + fromIntegral w)
    else unsafeChr (ord 'a' + (fromIntegral w) - 10)
