{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language UnboxedTuples #-}

-- This is broken out into a separate module to make it easier
-- to dump core and investigate performance issues.
module Data.Bytes.Search
  ( findIndices
  , replace
  , isInfixOf
  ) where

import Prelude hiding (length,takeWhile,dropWhile,null,foldl,foldr,elem,replicate,any,all,readFile,map)

import Control.Monad.ST.Run (runByteArrayST,runPrimArrayST)
import Data.Bits((.&.),(.|.),shiftL,finiteBitSize)
import Data.Bytes.Pure (length,unsafeIndex,unsafeHead)
import Data.Bytes.Types (Bytes(Bytes,array,offset))
import Data.Primitive (ByteArray,PrimArray)
import GHC.Exts (Int(I#))
import GHC.Word (Word32)

import qualified Data.Bytes.Byte as Byte
import qualified Data.Bytes.Pure as Pure
import qualified Data.Bytes.Types as Types
import qualified Data.Primitive as PM

-- Implementation Notes
-- =====================
-- For karp rabin, there are some easy performance improvements
-- left on the table. The main optimization that has been done is making
-- sure that there is no unnecessary boxing of Int, Word32, or Bytes
-- going on. Here are some other things that have not been done:
--
-- * The hash is currently a Word32. It would be better to use either
--   Word or Word64 for this. We would need for hashKey to be different.
-- * In several places, we track an index into a Bytes. This index gets
--   repeatedly added to the base offset as we loop over the bytes. We
--   could instead track the true offset instead of repeatedly
--   recalculating it.

-- | Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
replace ::
     Bytes -- ^ needle, must not be empty
  -> Bytes -- ^ replacement
  -> Bytes -- ^ haystack
  -> Bytes
{-# noinline replace #-}
-- Implementation note: there is a lot of room to improve the performance
-- of this function.
replace !needle !replacement !haystack@Bytes{array=haystackArray,offset=haystackIndex,length=haystackLength}
  | Pure.length needle == 0 = errorWithoutStackTrace "Data.Bytes.replace: needle of length zero"
  | Pure.length haystack == 0 = Pure.empty
  | Pure.length needle == 1, Pure.length replacement == 1 =
      let !needle0 = unsafeIndex needle 0
          !replacement0 = unsafeIndex replacement 0
       in Pure.map (\w -> if w == needle0 then replacement0 else w) haystack
  | otherwise =
      let !hp = rollingHash needle
          !ixs = findIndicesKarpRabin 0 hp needle haystackArray haystackIndex haystackLength
       in Pure.fromByteArray (replaceIndices ixs replacement (Pure.length needle) haystackArray haystackIndex haystackLength)


-- This is an internal function because it deals explicitly with
-- an offset into a byte array.
--
-- Example:
-- * haystack len: 39
-- * ixs: 7, 19, 33
-- * patLen: 5
-- * replacment: foo (len 3)
-- We want to perform these copies:
-- * src[0,7] -> dst[0,7]
-- * foo -> dst[7,3]
-- * src[12,7] -> dst[10,7]
-- * foo -> dst[17,3]
-- * src[24,9] -> dst[20,9]
-- * foo -> dst[29,3]
-- * src[38,1] -> dst[32,1]
replaceIndices :: PrimArray Int -> Bytes -> Int -> ByteArray -> Int -> Int -> ByteArray
replaceIndices !ixs !replacement !patLen !haystack !ix0 !len0 = runByteArrayST $ do
  let !ixsLen = PM.sizeofPrimArray ixs
  let !delta = Pure.length replacement - patLen
  dst <- PM.newByteArray (len0 + ixsLen * delta)
  let applyReplacement !ixIx !prevSrcIx = if ixIx < ixsLen
        then do
          let !srcMatchIx = PM.indexPrimArray ixs ixIx
          let !offset = ixIx * delta
          let !dstIx = srcMatchIx + offset - ix0
          Pure.unsafeCopy dst (prevSrcIx + offset - ix0)
            Bytes{array=haystack,offset=prevSrcIx,length=srcMatchIx - prevSrcIx}
          Pure.unsafeCopy dst dstIx replacement
          applyReplacement (ixIx + 1) (srcMatchIx + patLen)
        else do 
          let !offset = ixIx * delta
          Pure.unsafeCopy dst (prevSrcIx + offset - ix0)
            Bytes{array=haystack,offset=prevSrcIx,length=(len0 + ix0) - prevSrcIx}
          PM.unsafeFreezeByteArray dst
  applyReplacement 0 ix0

-- | Find locations of non-overlapping instances of @needle@ within @haystack@.
findIndices ::
     Bytes -- ^ needle
  -> Bytes -- ^ haystack
  -> PrimArray Int
findIndices needle Bytes{array,offset=off,length=len}
  | needleLen == 0 = errorWithoutStackTrace "Data.Bytes.findIndices: needle with length zero"
  | len == 0 = mempty
  | otherwise = 
      let !hp = rollingHash needle
       in findIndicesKarpRabin (negate off) hp needle array off len
  where
  needleLen = Pure.length needle

-- Precondition: Haystack has non-zero length
-- Precondition: Pattern has non-zero length
-- Uses karp rabin to search. 
-- Easy opportunity to improve implementation. Instead of having karpRabin
-- return two slices, we could have it just return a single index.
findIndicesKarpRabin ::
     Int -- Output index modifier. Set to negated initial index to make slicing invisible in results.
  -> Word32 -- Hash to search for (must agree with pattern)
  -> Bytes -- Pattern to search for
  -> ByteArray
  -> Int -- initial index
  -> Int -- length
  -> PrimArray Int
findIndicesKarpRabin !ixModifier !hp !pat !haystack !ix0 !len0 = runPrimArrayST $ do
  let dstLen = 1 + quot len0 (Pure.length pat)
  dst <- PM.newPrimArray dstLen
  let go !ix !len !ixIx = case karpRabin hp pat Bytes{array=haystack,offset=ix,length=len} of
        (-1) -> do
          PM.shrinkMutablePrimArray dst ixIx
          PM.unsafeFreezePrimArray dst
        skipCount -> do
          let !advancement = skipCount - Pure.length pat
          let !advancement' = advancement + Pure.length pat
          PM.writePrimArray dst ixIx (ix + advancement + ixModifier)
          let !ix' = ix + advancement'
          go ix' (len - advancement') (ixIx + 1)
  go ix0 len0 0

-- Output: Negative one means match not found. Other negative
-- numbers should not occur. Zero may occur. Positive number
-- means the number of bytes to skip to make it past the match.
breakSubstring :: Bytes -- ^ String to search for
               -> Bytes -- ^ String to search in
               -> Int
breakSubstring !pat !haystack@(Bytes _ off0 _) =
  case lp of
    0 -> 0
    1 -> case Byte.elemIndexLoop# (unsafeHead pat) haystack of
      (-1#) -> (-1)
      off -> 1 + (I# off) - off0
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
      then shift haystack
      else karpRabin (rollingHash pat) pat haystack
  where
  lp                = length pat
  {-# INLINE shift #-}
  shift :: Bytes -> Int
  shift !src
      | length src < lp = (-1)
      | otherwise       = search (intoWord $ Pure.unsafeTake lp src) lp
    where
    intoWord :: Bytes -> Word
    intoWord = Pure.foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
    wp   = intoWord pat
    mask = (1 `shiftL` (8 * lp)) - 1
    search :: Word -> Int -> Int
    search !w !i
        | w == wp         = i
        | length src <= i = (-1)
        | otherwise       = search w' (i + 1)
      where
      b  = fromIntegral (Pure.unsafeIndex src i)
      w' = mask .&. ((w `shiftL` 8) .|. b)

-- Only used for karp rabin
rollingHash :: Bytes -> Word32
{-# inline rollingHash #-}
rollingHash = Pure.foldl' (\h b -> h * hashKey + fromIntegral b) 0

hashKey :: Word32 
{-# inline hashKey #-}
hashKey = 2891336453

-- Precondition: Length of bytes is greater than or equal to 1.
-- Precondition: Rolling hash agrees with pattern.
-- Output: Negative one means match not found. Other negative
-- numbers should not occur. Zero should not occur. Positive number
-- means the number of bytes to skip to make it past the match.
karpRabin :: Word32 -> Bytes -> Bytes -> Int
karpRabin !hp !pat !src
    | length src < lp = (-1)
    | otherwise = search (rollingHash $ Pure.unsafeTake lp src) lp
  where
  lp :: Int
  !lp = Pure.length pat
  m :: Word32
  !m = hashKey ^ lp
  get :: Int -> Word32
  get !ix = fromIntegral (Pure.unsafeIndex src ix)
  search !hs !i
      | hp == hs && eqBytesNoShortCut pat (Pure.unsafeTake lp (Pure.unsafeDrop (i - lp) src)) = i
      | length src <= i                    = (-1)
      | otherwise                          = search hs' (i + 1)
    where
    hs' = hs * hashKey +
          get i -
          m * get (i - lp)

-- | Is the first argument an infix of the second argument?
-- 
-- Uses the Rabin-Karp algorithm: expected time @O(n+m)@, worst-case @O(nm)@.
isInfixOf :: Bytes -- ^ String to search for
          -> Bytes -- ^ String to search in
          -> Bool
isInfixOf p s = Pure.null p || breakSubstring p s >= 0


-- Precondition: both arguments have the same length
-- Skips the pointer equality check and the length check.
eqBytesNoShortCut :: Bytes -> Bytes -> Bool
{-# inline eqBytesNoShortCut #-}
eqBytesNoShortCut (Bytes arr1 off1 len1) (Bytes arr2 off2 _) =
  PM.compareByteArrays arr1 off1 arr2 off2 len1 == EQ
