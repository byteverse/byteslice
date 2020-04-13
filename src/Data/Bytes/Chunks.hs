{-# language BangPatterns #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UnboxedTuples #-}

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
    -- * Folds
  , foldl'
    -- * Splitting
  , split
    -- * Hashing
  , fnv1a32
  , fnv1a64
    -- * Create
  , fromBytes
  , fromByteArray
    -- * Copy to buffer
  , unsafeCopy
    -- * I\/O with Handles
  , hGetContents
  , readFile
  , hPut
  , writeFile
  ) where

import Prelude hiding (length,concat,reverse,readFile,writeFile)

import Control.Exception (IOException,catch)
import Control.Monad.ST.Run (runIntByteArrayST)
import Data.Bits (xor)
import Data.Bytes.Types (Bytes(Bytes))
import Data.Word (Word8,Word32,Word64)
import Data.Primitive (ByteArray(..),MutableByteArray(..))
import GHC.Exts (ByteArray#,MutableByteArray#)
import GHC.Exts (Int#,State#,Int(I#),(+#))
import GHC.ST (ST(..))
import System.IO (Handle,hFileSize,IOMode(ReadMode,WriteMode),withBinaryFile)

import qualified GHC.Exts as Exts
import qualified Data.Primitive as PM
import qualified Data.Bytes.Types as B
import qualified Data.Bytes.Pure as Bytes
import qualified Data.Bytes.Byte as Byte
import qualified Data.Bytes.IO as IO

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
hGetContents !h = hGetContentsCommon ChunksNil h

-- | Read a handle's entire contents strictly into chunks.
hGetContentsHint :: Int -> Handle -> IO Chunks
hGetContentsHint !hint !h = do
  c <- IO.hGet h hint
  let !r = ChunksCons c ChunksNil
  if Bytes.length c == hint
    then pure r
    else hGetContentsCommon r h

hGetContentsCommon ::
     Chunks -- reversed chunks
  -> Handle
  -> IO Chunks
hGetContentsCommon !acc0 !h = go acc0 where
  go !acc = do
    c <- IO.hGet h chunkSize
    let !r = ChunksCons c acc
    if Bytes.length c == chunkSize
      then go r
      else pure $! reverse r

-- | Read an entire file strictly into chunks. If reading from a
-- regular file, this makes an effort read the file into a single
-- chunk.
readFile :: FilePath -> IO Chunks
readFile f = withBinaryFile f ReadMode $ \h -> do
  -- Implementation copied from bytestring.
  -- hFileSize fails if file is not regular file (like
  -- /dev/null). Catch exception and try reading anyway.
  filesz <- catch (hFileSize h) useZeroIfNotRegularFile
  let hint = (fromIntegral filesz `max` 255) + 1
  hGetContentsHint hint h
  -- Our initial size is one bigger than the file size so that in the
  -- typical case we will read the whole file in one go and not have
  -- to allocate any more chunks. We'll still do the right thing if the
  -- file size is 0 or is changed before we do the read.
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0

chunkSize :: Int
chunkSize = 16384 - 16

-- | Create a list of chunks with a single chunk.
fromBytes :: Bytes -> Chunks
fromBytes !b = ChunksCons b ChunksNil

-- | Variant of 'fromBytes' where the single chunk is unsliced.
fromByteArray :: ByteArray -> Chunks
fromByteArray !b = fromBytes (Bytes.fromByteArray b)

-- | Left fold over all bytes in the chunks, strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> Chunks -> a
{-# inline foldl' #-}
foldl' g = go where
  go !a ChunksNil = a
  go !a (ChunksCons c cs) = go (Bytes.foldl' g a c) cs

-- | Hash byte sequence with 32-bit variant of FNV-1a.
fnv1a32 :: Chunks -> Word32
fnv1a32 = foldl'
  (\acc w -> (fromIntegral @Word8 @Word32 w `xor` acc) * 0x01000193
  ) 0x811c9dc5

-- | Hash byte sequence with 64-bit variant of FNV-1a.
fnv1a64 :: Chunks -> Word64
fnv1a64 = foldl'
  (\acc w -> (fromIntegral @Word8 @Word64 w `xor` acc) * 0x00000100000001B3
  ) 0xcbf29ce484222325

-- | Outputs 'Chunks' to the specified 'Handle'. This is implemented
-- with 'hPutBuf'.
hPut :: Handle -> Chunks -> IO ()
hPut h = go where
  go ChunksNil = pure ()
  go (ChunksCons c cs) = IO.hPut h c *> go cs

-- | Write 'Chunks' to a file, replacing the previous contents of
-- the file.
writeFile :: FilePath -> Chunks -> IO ()
writeFile path cs = withBinaryFile path WriteMode (\h -> hPut h cs)

-- | Break chunks of bytes into contiguous pieces separated by the
-- byte argument. This is a good producer for list fusion. For this
-- function to perform well, each chunk should contain multiple separators.
-- Any piece that spans multiple chunks must be copied.
split :: Word8 -> Chunks -> [Bytes]
{-# inline split #-}
split !w !cs0 = Exts.build
  (\g x0 ->
    -- It is possible to optimize for the common case where a
    -- piece does not span multiple chunks. However, such an
    -- optimization would actually cause this to tail call in
    -- two places rather than one and may actually adversely
    -- affect performance. It hasn't been benchmarked.
    let go !cs = case splitOnto ChunksNil w cs of
          (hd,tl) -> let !x = concat (reverse hd) in
            case tl of
              ChunksNil -> x0
              _ -> g x (go tl)
     in go cs0
  )

splitOnto :: Chunks -> Word8 -> Chunks -> (Chunks,Chunks)
{-# inline splitOnto #-}
splitOnto !acc0 !w !cs0 = go acc0 cs0 where
  go !acc ChunksNil = (acc,ChunksNil)
  go !acc (ChunksCons b bs) = case Byte.split1 w b of
    Nothing -> go (ChunksCons b acc) bs
    Just (hd,tl) ->
      let !r1 = ChunksCons hd acc
          !r2 = ChunksCons tl bs
       in (r1,r2)
