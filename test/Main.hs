{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Bytes.Chunks (Chunks(ChunksNil,ChunksCons))
import Data.Bytes.Types (Bytes(Bytes))
import Data.Char (ord)
import Data.Primitive (ByteArray)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)
import Test.Tasty.QuickCheck ((===),testProperty,property,Discard(Discard))
import Test.Tasty.QuickCheck ((==>),Arbitrary)

import qualified Data.ByteString as ByteString
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.QuickCheck.Classes as QCC
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Bytes"
  [ testGroup "isPrefixOf"
    [ testCase "A" $ THU.assertBool "" $
        Bytes.isPrefixOf (bytes "hey") (bytes "hey man")
    , testCase "B" $ THU.assertBool "" $
        not (Bytes.isPrefixOf (bytes "an") (bytes "hey man"))
    ]
  , testGroup "isSuffixOf"
    [ testCase "A" $ THU.assertBool "" $
        Bytes.isSuffixOf (bytes "an") (bytes "hey man")
    , testCase "B" $ THU.assertBool "" $
        not (Bytes.isSuffixOf (bytes "h") (bytes "hey man"))
    ]
  , testGroup "stripOptionalSuffix"
    [ testCase "A" $
        Bytes.fromAsciiString "hey m"
        @=?
        Bytes.stripOptionalSuffix (bytes "an") (bytes "hey man")
    , testCase "B" $
        Bytes.fromAsciiString "hey man"
        @=?
        Bytes.stripOptionalSuffix (bytes "h") (bytes "hey man")
    ]
  , testGroup "dropWhileEnd"
    [ testCase "A" $
        Bytes.fromAsciiString "aabbcc"
        @=?
        Bytes.dropWhileEnd (== c2w 'b') (bytes "aabbccbb")
    ]
  , testGroup "takeWhileEnd"
    [ testCase "A" $
        Bytes.fromAsciiString "bb"
        @=?
        Bytes.takeWhileEnd (== c2w 'b') (bytes "aabbccbb")
    , testCase "B" $
        Bytes.fromAsciiString ""
        @=?
        Bytes.takeWhileEnd (/= c2w '\n') (bytes "aabbccbb\n")
    , testCase "C" $
        slicedPack [0x1,0x2,0x3]
        @=?
        Bytes.takeWhileEnd (/= 0x0) (slicedPack [0x1,0x0,0x1,0x2,0x3])
    ]
  , testProperty "elem" $ \(x :: Word8) (xs :: [Word8]) ->
      List.elem x xs
      ===
      Bytes.elem x (Bytes.unsafeDrop 1 (Exts.fromList (x : xs)))
  , testProperty "foldl" $ \(x :: Word8) (xs :: [Word8]) ->
      List.foldl (-) 0 xs
      ===
      Bytes.foldl (-) 0 (Bytes.unsafeDrop 1 (Exts.fromList (x : xs)))
  , testProperty "foldl'" $ \(x :: Word8) (xs :: [Word8]) ->
      List.foldl' (-) 0 xs
      ===
      Bytes.foldl' (-) 0 (Bytes.unsafeDrop 1 (Exts.fromList (x : xs)))
  , testProperty "foldr" $ \(x :: Word8) (xs :: [Word8]) ->
      Foldable.foldr (-) 0 xs
      ===
      Bytes.foldr (-) 0 (Bytes.unsafeDrop 1 (Exts.fromList (x : xs)))
  , testProperty "foldr'" $ \(x :: Word8) (xs :: [Word8]) ->
      Foldable.foldr' (-) 0 xs
      ===
      Bytes.foldr' (-) 0 (Bytes.unsafeDrop 1 (Exts.fromList (x : xs)))
  , testProperty "count" $ \(x :: Word8) (xs :: [Word8]) ->
      ByteString.count x (ByteString.pack xs)
      ===
      Bytes.count x (slicedPack xs)
  , testProperty "split" $ \(x :: Word8) (xs :: [Word8]) ->
      not (List.null xs)
      ==>
      ByteString.split x (ByteString.pack xs)
      ===
      map bytesToByteString (Bytes.split x (slicedPack xs))
  , testProperty "splitNonEmpty" $ \(x :: Word8) (xs :: [Word8]) ->
      Bytes.split x (slicedPack xs)
      ===
      Foldable.toList (Bytes.splitNonEmpty x (slicedPack xs))
  , testProperty "splitInit" $ \(x :: Word8) (xs :: [Word8]) -> case xs of
      [] -> Bytes.splitInit x (slicedPack xs) === []
      _ -> 
        List.init (ByteString.split x (ByteString.pack xs))
        ===
        map bytesToByteString (Bytes.splitInit x (slicedPack xs))
  , testProperty "splitU" $ \(x :: Word8) (xs :: [Word8]) ->
      not (List.null xs)
      ==>
      Bytes.split x (slicedPack xs)
      ===
      map Bytes.fromByteArray (Exts.toList (Bytes.splitU x (slicedPack xs)))
  , testCase "splitInit-A" $
      [Bytes.fromAsciiString "hello", Bytes.fromAsciiString "world"]
      @=?
      (Bytes.splitInit 0x0A (Bytes.fromAsciiString "hello\nworld\n"))
  , testCase "splitInit-B" $
      [Bytes.fromAsciiString "hello", Bytes.fromAsciiString "world"]
      @=?
      (Bytes.splitInit 0x0A (Bytes.fromAsciiString "hello\nworld\nthere"))
  , testProperty "split1" $ \(x :: Word8) (xs :: [Word8]) ->
      case ByteString.split x (ByteString.pack xs) of
        [] -> Bytes.split1 x (slicedPack xs) === Nothing
        [_] -> Bytes.split1 x (slicedPack xs) === Nothing
        [y1,z1] -> case Bytes.split1 x (slicedPack xs) of
          Nothing -> property False
          Just (y2,z2) -> (y1,z1) === (bytesToByteString y2, bytesToByteString z2)
        _ -> property Discard
  , testProperty "split2" $ \(xs :: [Word8]) (ys :: [Word8]) (zs :: [Word8]) ->
      (all (/=0xEF) xs && all (/=0xEF) ys && all (/=0xEF) zs)
      ==>
      case Bytes.split2 0xEF (slicedPack (xs ++ [0xEF] ++ ys ++ [0xEF] ++ zs)) of
        Just r -> r === (slicedPack xs, slicedPack ys, slicedPack zs)
        Nothing -> property False
  , testProperty "split3" $ \(ws :: [Word8]) (xs :: [Word8]) (ys :: [Word8]) (zs :: [Word8])->
      (all (/=0xEF) ws && all (/=0xEF) xs && all (/=0xEF) ys && all (/=0xEF) zs)
      ==>
      case Bytes.split3 0xEF (slicedPack (ws ++ [0xEF] ++ xs ++ [0xEF] ++ ys ++ [0xEF] ++ zs)) of
        Just r -> r === (slicedPack ws, slicedPack xs, slicedPack ys, slicedPack zs)
        Nothing -> property False
  , testGroup "Chunks"
    [ lawsToTest (QCC.eqLaws (Proxy :: Proxy Chunks))
    , lawsToTest (QCC.semigroupLaws (Proxy :: Proxy Chunks))
    , lawsToTest (QCC.monoidLaws (Proxy :: Proxy Chunks))
    , testGroup "concatenation"
      [ testProperty "concat=concatU" $ \(c :: Chunks) ->
          Chunks.concat c === Bytes.fromByteArray (Chunks.concatU c)
      , testProperty "concat-singleton" $ \(b :: Bytes) ->
          Chunks.concat (ChunksCons b ChunksNil) === b
      , testProperty "concatU-singleton" $ \(b :: Bytes) ->
          Chunks.concatU (ChunksCons b ChunksNil) === Bytes.toByteArray b
      ]
    ]
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

slicedPack :: [Word8] -> Bytes
slicedPack s =
  let b = Exts.fromList ([0x00] ++ s ++ [0x00])
   in Bytes b 1 (PM.sizeofByteArray b - 2)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

c2w :: Char -> Word8
c2w = fromIntegral . ord

bytesToByteString :: Bytes -> ByteString.ByteString
bytesToByteString = ByteString.pack . Bytes.foldr (:) []

instance Arbitrary Bytes where
  arbitrary = do
    xs :: [Word8] <- TQC.arbitrary
    front <- TQC.choose (0,2)
    back <- TQC.choose (0,2)
    let frontPad = replicate front (254 :: Word8)
    let backPad = replicate back (254 :: Word8)
    let raw = Exts.fromList (frontPad ++ xs ++ backPad)
    pure (Bytes raw front (length xs))

instance Arbitrary Chunks where
  arbitrary = do
    xs :: [[Word8]] <- TQC.arbitrary
    let ys = map
          (\x -> Exts.fromList ([255] ++ x ++ [255]))
          xs
        zs = foldr
          (\b cs ->
            ChunksCons (Bytes b 1 (PM.sizeofByteArray b - 2)) cs
          ) ChunksNil ys
    pure zs

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)
