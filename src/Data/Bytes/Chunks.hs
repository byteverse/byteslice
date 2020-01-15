{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language NamedFieldPuns #-}

-- | Chunks of bytes. This is useful as a target for a builder
-- or as a way to read a large amount of whose size is unknown
-- in advance. Structurally, this type is similar to
-- @Data.ByteString.Lazy.ByteString@. However, the type in this
-- module is strict in its spine. Additionally, none of the
-- @Handle@ functions perform lazy I\/O.
module Data.Bytes.Chunks
  ( -- * Types
    Chunks(..)
    -- * Properties
  , length
    -- * Manipulate
  , concat
  , reverse
  , reverseOnto
    -- * I\/O with Handles
  , hGetContents
  ) where

import Prelude hiding (length,concat,reverse)

import Control.Monad.ST.Run (runByteArrayST)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.Exts (Int#,State#,Int(I#),(+#))
import GHC.ST (ST(..))
import System.IO (Handle)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Data.Bytes.Types as B
import qualified Data.Bytes as Bytes

data Chunks
  = ChunksCons {-# UNPACK #-} !Bytes !Chunks
  | ChunksNil
  deriving stock (Show)

instance Semigroup Chunks where
  ChunksNil <> a = a
  cs@(ChunksCons _ _) <> ChunksNil = cs
  as@(ChunksCons _ _) <> bs@(ChunksCons _ _) =
    reverseOnto bs (reverse as)

instance Monoid Chunks where
  mempty = ChunksNil

instance Eq Chunks where
  -- TODO: There is a more efficient way to do this, but
  -- it is tedious.
  a == b = concat a == concat b

concat :: Chunks -> ByteArray
concat x = ByteArray (concat# x)

concat# :: Chunks -> ByteArray#
{-# noinline concat# #-}
concat# ChunksNil = case mempty of {ByteArray x -> x}
concat# (ChunksCons (Bytes{array=c,offset=coff,length=szc}) cs) = case cs of
  ChunksNil -> case c of {ByteArray x -> x}
  ChunksCons (Bytes{array=d,offset=doff,length=szd}) ds ->
    unBa $ runByteArrayST $ do
      let szboth = szc + szd
          len = chunksLengthGo szboth ds
      dst <- PM.newByteArray len
      PM.copyByteArray dst 0 c coff szc
      PM.copyByteArray dst szc d doff szd
      _ <- copy dst szboth ds
      PM.unsafeFreezeByteArray dst

length :: Chunks -> Int
length = chunksLengthGo 0

chunksLengthGo :: Int -> Chunks -> Int
chunksLengthGo !n ChunksNil = n
chunksLengthGo !n (ChunksCons (Bytes{B.length=len}) cs) =
  chunksLengthGo (n + len) cs

-- | Copy the contents of the chunks into a mutable array.
-- Precondition: The destination must have enough space to
-- house the contents. This is not checked.
copy ::
     MutableByteArray s -- ^ Destination
  -> Int -- ^ Destination offset
  -> Chunks -- ^ Source
  -> ST s Int -- ^ Returns the next index into the destination after the payload
{-# inline copy #-}
copy (MutableByteArray dst) (I# off) cs = ST
  (\s0 -> case copy# dst off cs s0 of
    (# s1, nextOff #) -> (# s1, I# nextOff #)
  )

copy# :: MutableByteArray# s -> Int# -> Chunks -> State# s -> (# State# s, Int# #)
copy# _ off ChunksNil s0 = (# s0, off #)
copy# marr off (ChunksCons (Bytes{B.array,B.offset,B.length=len}) cs) s0 =
  case Exts.copyByteArray# (unBa array) (unI offset) marr off (unI len) s0 of
    s1 -> copy# marr (off +# unI len) cs s1


-- | Reverse chunks but not the bytes within each chunk.
reverse :: Chunks -> Chunks
reverse = reverseOnto ChunksNil

-- | Variant of 'reverse' that allows the caller to provide
-- an initial list of chunks that the reversed chunks will
-- be pushed onto.
reverseOnto :: Chunks -> Chunks -> Chunks
reverseOnto !x ChunksNil = x
reverseOnto !x (ChunksCons y ys) =
  reverseOnto (ChunksCons y x) ys

unI :: Int -> Int#
unI (I# i) = i

unBa :: ByteArray -> ByteArray#
unBa (ByteArray x) = x

-- | Read a handle's entire contents strictly into chunks.
hGetContents :: Handle -> IO Chunks
hGetContents !h = do
  result <- go ChunksNil
  pure $! reverse result
  where
  go !acc = do
    c <- Bytes.hGet h chunkSize
    let !r = ChunksCons c acc
    if Bytes.length c == chunkSize
      then go r
      else pure r

chunkSize :: Int
chunkSize = 16384 - 16
