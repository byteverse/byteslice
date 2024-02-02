import Gauge.Main (bench, bgroup, defaultMain, whnf)

import Data.Bytes.Types
import Data.List (permutations)

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Ascii as Ascii

naiveMconcat :: [Bytes] -> Bytes
naiveMconcat = foldr mappend mempty

main :: IO ()
main =
  defaultMain
    [ bench "mconcat" $ whnf mconcat mconcatBytes
    , bench "naiveMconcat" $ whnf naiveMconcat mconcatBytes
    , bgroup
        "replace"
        [ bench "the-dog-and-the-shadow" (whnf replaceMeat theDogAndTheShadow)
        ]
    ]

mconcatBytes :: [Bytes]
mconcatBytes = fmap Ascii.fromString $ permutations ['a' .. 'g']

replaceMeat :: Bytes -> Bytes
{-# NOINLINE replaceMeat #-}
replaceMeat x = Bytes.replace meat gruel x

meat :: Bytes
meat = Ascii.fromString "meat"

gruel :: Bytes
gruel = Ascii.fromString "gruel"

theDogAndTheShadow :: Bytes
theDogAndTheShadow =
  Ascii.fromString $
    concat
      [ "It happened that a Dog had got a piece of meat and was "
      , "carrying it home in his mouth to eat it in peace. "
      , "Now on his way home he had to cross a plank lying across "
      , "a running brook. As he crossed, he looked down and saw his "
      , "own shadow reflected in the water beneath. Thinking it was "
      , "another dog with another piece of meat, he made up his mind "
      , "to have that also. So he made a snap at the shadow in the water, "
      , "but as he opened his mouth the piece of meat fell out, dropped "
      , "into the water and was never seen more."
      ]
