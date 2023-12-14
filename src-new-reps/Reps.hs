{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module Reps
  ( Bytes#(..)
  , word8ToWord#
  ) where

import GHC.Exts (ByteArray#,Int#,RuntimeRep(..),Levity(Unlifted),TYPE,word8ToWord#)

newtype Bytes# :: TYPE ('TupleRep '[ 'BoxedRep 'Unlifted,'IntRep,'IntRep]) where
  Bytes# :: (# ByteArray#, Int#, Int# #) -> Bytes#
