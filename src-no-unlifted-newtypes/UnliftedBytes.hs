{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}

module UnliftedBytes
  ( Bytes#
  , lift
  , unlift
  ) where

import Data.Bytes.Internal (Bytes)
import GHC.TypeLits
import GHC.Exts (RuntimeRep(..),TYPE)

type family Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# = TypeError ('Text "Bytes# not available before GHC 8.10")

lift :: Bytes# -> Bytes
lift _ = errorWithoutStackTrace "UnliftedBytes: lift not implemented"

unlift :: Bytes -> Bytes#
unlift _ = errorWithoutStackTrace "UnliftedBytes: unlift not implemented"
