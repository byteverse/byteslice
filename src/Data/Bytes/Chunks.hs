{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}

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
  , concatPinned
  , concatU
  , reverse
  , reverseOnto
    -- * Create
  , fromBytes
  , fromByteArray
    -- * Copy to buffer
  , unsafeCopy
    -- * I\/O with Handles
  , hGetContents
  ) where

import Prelude hiding (length,concat,reverse)

import Control.Monad.ST.Run (runIntByteArrayST)
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

-- | A cons-list of byte sequences.
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

-- | This uses @concat@ to form an equivalence class.
instance Eq Chunks where
  -- TODO: There is a more efficient way to do this, but
  -- it is tedious.
  a == b = concat a == concat b

-- | Variant of 'concat' that ensure that the resulting byte
-- sequence is pinned memory.
concatPinned :: Chunks -> Bytes
concatPinned x = case x of
  ChunksNil -> Bytes.emptyPinned
  ChunksCons b y -> case y of
    ChunksNil -> Bytes.pin b
    ChunksCons c z -> case concatPinnedFollowing2 b c z of
      (# len, r #) -> Bytes (ByteArray r) 0 (I# len)

-- | Concatenate chunks into a single contiguous byte sequence.
concat :: Chunks -> Bytes
concat x = case x of
  ChunksNil -> Bytes.empty
  ChunksCons b y -> case y of
    ChunksNil -> b
    ChunksCons c z -> case concatFollowing2 b c z of
      (# len, r #) -> Bytes (ByteArray r) 0 (I# len)

-- | Variant of 'concat' that returns an unsliced byte sequence.
concatU :: Chunks -> ByteArray
concatU x = case x of
  ChunksNil -> mempty
  ChunksCons b y -> case y of
    ChunksNil -> Bytes.toByteArray b
    ChunksCons c z -> case concatFollowing2 b c z of
      (# _, r #) -> ByteArray r

concatFollowing2 :: Bytes -> Bytes -> Chunks -> (# Int#, ByteArray# #)
concatFollowing2 = internalConcatFollowing2 PM.newByteArray

concatPinnedFollowing2 :: Bytes -> Bytes -> Chunks -> (# Int#, ByteArray# #)
concatPinnedFollowing2 = internalConcatFollowing2 PM.newPinnedByteArray

internalConcatFollowing2 ::
     (forall s. Int -> ST s (MutableByteArray s))
  -> Bytes
  -> Bytes
  -> Chunks
  -> (# Int#, ByteArray# #)
{-# inline internalConcatFollowing2 #-}
internalConcatFollowing2 allocate
  (Bytes{array=c,offset=coff,length=szc}) 
  (Bytes{array=d,offset=doff,length=szd}) ds =
    let !(I# x, ByteArray y) = runIntByteArrayST $ do
          let !szboth = szc + szd
              !len = chunksLengthGo szboth ds
          dst <- allocate len
          PM.copyByteArray dst 0 c coff szc
          PM.copyByteArray dst szc d doff szd
          -- Note: len2 will always be the same as len.
          !len2 <- unsafeCopy dst szboth ds
          result <- PM.unsafeFreezeByteArray dst
          pure (len2,result)
     in (# x, y #)

-- | The total number of bytes in all the chunks.
length :: Chunks -> Int
length = chunksLengthGo 0

chunksLengthGo :: Int -> Chunks -> Int
chunksLengthGo !n ChunksNil = n
chunksLengthGo !n (ChunksCons (Bytes{B.length=len}) cs) =
  chunksLengthGo (n + len) cs

-- | Copy the contents of the chunks into a mutable array.
-- Precondition: The destination must have enough space to
-- house the contents. This is not checked.
unsafeCopy ::
     MutableByteArray s -- ^ Destination
  -> Int -- ^ Destination offset
  -> Chunks -- ^ Source
  -> ST s Int -- ^ Returns the next index into the destination after the payload
{-# inline unsafeCopy #-}
unsafeCopy (MutableByteArray dst) (I# off) cs = ST
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

-- | Create a list of chunks with a single chunk.
fromBytes :: Bytes -> Chunks
fromBytes !b = ChunksCons b ChunksNil

-- | Variant of 'fromBytes' where the single chunk is unsliced.
fromByteArray :: ByteArray -> Chunks
fromByteArray !b = fromBytes (Bytes.fromByteArray b)
