{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module UnliftedBytes
  ( Bytes#(..)
  , lift
  , unlift
  ) where

import Data.Bytes.Internal (Bytes(Bytes))
import Data.Primitive (ByteArray(ByteArray))
import GHC.Exts (Int(I#),ByteArray#,Int#,RuntimeRep(..),TYPE)

newtype Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#

lift :: Bytes# -> Bytes
lift (Bytes# (# arr, off, len #)) = Bytes (ByteArray arr) (I# off) (I# len)

unlift :: Bytes -> Bytes#
unlift (Bytes (ByteArray arr) (I# off) (I# len)) =
  Bytes# (# arr, off, len #)
