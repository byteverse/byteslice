{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language UndecidableInstances #-}
{-# language UnliftedFFITypes #-}

module UnliftedBytes
  ( Bytes#
  , cstringLength#
  ) where

import GHC.TypeLits
import GHC.Exts (Addr#,Int#,RuntimeRep(..),TYPE)

type family Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# = TypeError ('Text "Bytes# not available before GHC 8.10")

foreign import ccall unsafe "strlen" c_strlen :: Addr# -> Int#

cstringLength# :: Addr# -> Int#
cstringLength# = c_strlen
