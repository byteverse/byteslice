{-# language MagicHash #-}
{-# language TypeFamilies #-}
{-# language UnliftedFFITypes #-}

module Cstrlen
  ( cstringLength#
  ) where

import GHC.Exts (Addr#,Int#)

foreign import ccall unsafe "strlen" c_strlen :: Addr# -> Int#

cstringLength# :: Addr# -> Int#
cstringLength# = c_strlen
