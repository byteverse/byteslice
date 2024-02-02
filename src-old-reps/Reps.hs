{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Reps
  ( Bytes# (..)
  , word8ToWord#
  ) where

import GHC.Exts (ByteArray#, Int#, RuntimeRep (..), TYPE, Word#)

newtype Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep, 'IntRep, 'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#

-- In GHC 9.2, the lifted Word8 type started being backed by the
-- unlifted Word8# instead of by Word#. This is a compatibility hack.
word8ToWord# :: Word# -> Word#
{-# INLINE word8ToWord# #-}
word8ToWord# w = w
