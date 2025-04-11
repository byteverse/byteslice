{-# language DataKinds #-}
{-# language MagicHash #-}
{-# language TypeOperators #-}

module Data.Bytes.Indexed
  ( ByteArrayN
  , append
  , length
  , length#
  ) where

import Prelude hiding (length)

import Data.Primitive (ByteArray(ByteArray))
import Data.Bytes.Types (ByteArrayN(ByteArrayN))
import GHC.TypeNats (type (+))
import Arithmetic.Types (Nat, Nat#)

import qualified Data.Primitive as PM
import qualified Arithmetic.Unsafe as Unsafe
import qualified GHC.Exts as Exts

append :: ByteArrayN m -> ByteArrayN n -> ByteArrayN (m + n)
{-# inline append #-}
append (ByteArrayN x) (ByteArrayN y) = ByteArrayN (x <> y)

-- | Recover a witness of the length.
length :: ByteArrayN n -> Nat n
{-# inline length #-}
length (ByteArrayN x) = Unsafe.Nat (PM.sizeofByteArray x)

-- | Recover an unboxed witness of the length.
length# :: ByteArrayN n -> Nat# n
{-# inline length# #-}
length# (ByteArrayN (ByteArray x)) = Unsafe.Nat# (Exts.sizeofByteArray# x)
