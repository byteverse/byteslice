{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language TypeInType #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Reps
  ( Bytes#(..)
  , word8ToWord#
  ) where

import GHC.Exts (ByteArray#,Int#,Word#,RuntimeRep(..),TYPE)

newtype Bytes# :: TYPE ('TupleRep '[ 'UnliftedRep,'IntRep,'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#

-- In GHC 9.2, the lifted Word8 type started being backed by the
-- unlifted Word8# instead of by Word#. This is a compatibility hack.
word8ToWord# :: Word# -> Word#
{-# inline word8ToWord# #-}
word8ToWord# w = w
