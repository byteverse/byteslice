{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Reps
  ( Bytes# (..)
  , word8ToWord#
  ) where

import GHC.Exts (ByteArray#, Int#, Levity (Unlifted), RuntimeRep (..), TYPE, word8ToWord#)

newtype Bytes# :: TYPE ('TupleRep '[ 'BoxedRep 'Unlifted, 'IntRep, 'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#
