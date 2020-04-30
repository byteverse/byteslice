{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}

module UnliftedBytes
  ( Bytes#
  ) where

import GHC.TypeLits
import GHC.Exts (RuntimeRep(..),TYPE)

type family Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# = TypeError ('Text "Bytes# not available before GHC 8.10")
