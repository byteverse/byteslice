{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Bytes.Types
  ( Bytes (..)
  , Bytes# (..)
  , MutableBytes (..)
  , UnmanagedBytes (..)
  , BytesN (..)
  , ByteArrayN (..)
  ) where

import Data.Bytes.Internal (Bytes (..))
import Data.Bytes.Internal.Show (showsSlice)
import Data.Primitive (ByteArray (..), MutableByteArray (..))
import Data.Primitive.Addr (Addr)
import Data.Proxy (Proxy (Proxy))
import GHC.Natural (naturalToInteger)
import GHC.TypeNats (KnownNat, Nat, natVal)
import Reps (Bytes# (..))

{- | A slice of a 'ByteArray' whose compile-time-known length is represented
by a phantom type variable. Consumers of this data constructor must be
careful to preserve the expected invariant.
-}
data BytesN (n :: Nat) = BytesN
  { array :: {-# UNPACK #-} !ByteArray
  , offset :: {-# UNPACK #-} !Int
  }

instance (KnownNat n) => Show (BytesN n) where
  showsPrec _ (BytesN arr off) s =
    let len = fromInteger (naturalToInteger (natVal (Proxy :: Proxy n)))
     in showsSlice arr off len s

{- | A 'ByteArray' whose compile-time-known length is represented
by a phantom type variable. Consumers of this data constructor must be
careful to preserve the expected invariant.
-}
newtype ByteArrayN (n :: Nat) = ByteArrayN
  { array :: ByteArray
  }

instance (KnownNat n) => Show (ByteArrayN n) where
  showsPrec _ (ByteArrayN arr) s =
    let len = fromInteger (naturalToInteger (natVal (Proxy :: Proxy n)))
     in showsSlice arr 0 len s

-- | A slice of a 'MutableByteArray'.
data MutableBytes s = MutableBytes
  { array :: {-# UNPACK #-} !(MutableByteArray s)
  , offset :: {-# UNPACK #-} !Int
  , length :: {-# UNPACK #-} !Int
  }

-- | A slice of unmanaged memory.
data UnmanagedBytes = UnmanagedBytes
  { address :: {-# UNPACK #-} !Addr
  , length :: {-# UNPACK #-} !Int
  }
