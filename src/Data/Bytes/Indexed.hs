{-# language DataKinds #-}
{-# language TypeOperators #-}

module Data.Bytes.Indexed
  ( ByteArrayN
  , append
  ) where

import Data.Bytes.Types (ByteArrayN(ByteArrayN))
import GHC.TypeNats (type (+))

append :: ByteArrayN m -> ByteArrayN n -> ByteArrayN (m + n)
{-# inline append #-}
append (ByteArrayN x) (ByteArrayN y) = ByteArrayN (x <> y)
