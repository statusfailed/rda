module RDA.ReverseDerivative
  ( rdiffB
  ) where

import RDA.Nat
import RDA.BitVec
import RDA.Bitwise (cmpz)
import Data.Bits
import Data.Proxy
import GHC.TypeNats

-- | Reverse Derivative, brute force implementation.
-- @rd f@ computes the reverse derivative using @n + 1@ applications of @f@
rdiffB :: forall n m . (KnownNat n, KnownNat m)
  => (BitVec Integer n -> BitVec Integer m)
  -> BitVec Integer (n + m)
  -> BitVec Integer n
rdiffB f v = concatBits @n (parity <$> partials)
  where
    (x, dy)  = split @n @m v
    partials = [ (f x `xor` f (x `xor` e_i)) .&. dy | e_i <- fmap bit [0..nat @n - 1] ]
