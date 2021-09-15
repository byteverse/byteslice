import Gauge.Main (defaultMain, bench, whnf)

import Data.Bytes.Types
import Data.List (permutations)
import qualified Data.Bytes.Text.Ascii as Ascii

naiveMconcat :: [Bytes] -> Bytes
naiveMconcat = foldr mappend mempty

main :: IO ()
main = defaultMain
  [ bench "mconcat" $ whnf mconcat mconcatBytes
  , bench "naiveMconcat" $ whnf naiveMconcat mconcatBytes
  ]

mconcatBytes :: [Bytes]
mconcatBytes = fmap Ascii.fromString $ permutations ['a'..'g']
