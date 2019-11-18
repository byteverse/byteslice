{-# language BangPatterns #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

import Data.Primitive (ByteArray)
import Data.Word (Word8)
import Data.Char (ord)
import Data.Bytes.Types (Bytes(Bytes))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?),testCase)
import Test.Tasty.QuickCheck ((===),testProperty,property,Discard(Discard))
import Test.Tasty.QuickCheck ((==>))

import qualified Data.Bytes as Bytes
import qualified Data.ByteString as ByteString
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU

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
  , testProperty "split1" $ \(x :: Word8) (xs :: [Word8]) ->
      Bytes.split x (slicedPack xs)
      ===
      Foldable.toList (Bytes.split1 x (slicedPack xs))
  , testProperty "splitInit" $ \(x :: Word8) (xs :: [Word8]) -> case xs of
      [] -> Bytes.splitInit x (slicedPack xs) === []
      _ -> 
        List.init (ByteString.split x (ByteString.pack xs))
        ===
        map bytesToByteString (Bytes.splitInit x (slicedPack xs))
  , testCase "splitInit-A" $
      [Bytes.fromAsciiString "hello", Bytes.fromAsciiString "world"]
      @=?
      (Bytes.splitInit 0x0A (Bytes.fromAsciiString "hello\nworld\n"))
  , testCase "splitInit-B" $
      [Bytes.fromAsciiString "hello", Bytes.fromAsciiString "world"]
      @=?
      (Bytes.splitInit 0x0A (Bytes.fromAsciiString "hello\nworld\nthere"))
  , testProperty "splitOnce" $ \(x :: Word8) (xs :: [Word8]) ->
      case ByteString.split x (ByteString.pack xs) of
        [] -> Bytes.splitOnce x (slicedPack xs) === Nothing
        [_] -> Bytes.splitOnce x (slicedPack xs) === Nothing
        [y1,z1] -> case Bytes.splitOnce x (slicedPack xs) of
          Nothing -> property False
          Just (y2,z2) -> (y1,z1) === (bytesToByteString y2, bytesToByteString z2)
        _ -> property Discard
  , testProperty "splitTwice" $ \(xs :: [Word8]) (ys :: [Word8]) (zs :: [Word8]) ->
      (all (/=0xEF) xs && all (/=0xEF) ys && all (/=0xEF) zs)
      ==>
      case Bytes.splitTwice 0xEF (slicedPack (xs ++ [0xEF] ++ ys ++ [0xEF] ++ zs)) of
        Just r -> r === (slicedPack xs, slicedPack ys, slicedPack zs)
        Nothing -> property False
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
