{-# language MagicHash #-}
{-# language UnliftedFFITypes #-}

module Data.Bytes.Compat
  ( cstringLength#
  ) where

import GHC.Exts

-- GHC 8.12 comes with a known-key implementation of strlen that supports
-- constant folding. Here we define a shim for older GHCs.

foreign import ccall unsafe "strlen" c_strlen :: Addr# -> Int#

cstringLength# :: Addr# -> Int#
cstringLength# = c_strlen
