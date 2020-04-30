{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module UnliftedBytes
  ( Bytes#(..)
  ) where

import GHC.Exts (ByteArray#,Int#,RuntimeRep(..),TYPE)

newtype Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#
