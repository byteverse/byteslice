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
import Test.Tasty.QuickCheck ((===),testProperty)

import qualified Data.Bytes as Bytes
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Test.Tasty.HUnit as THU
import qualified Test.Tasty.QuickCheck as QC

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
  ]

bytes :: String -> Bytes
bytes s = let b = pack ('x' : s) in Bytes b 1 (PM.sizeofByteArray b - 1)

pack :: String -> ByteArray
pack = Exts.fromList . map (fromIntegral @Int @Word8 . ord)

