module RDA.ReverseDerivative
  ( eval
  , partial
  , component
  , rd1
  ) where

import RDA.BitVec
import RDA.Bitwise (cmpz)
import Data.Bits
import Data.Proxy
import GHC.TypeNats

-- | Evaluate a truth table of @2^n@ bits as a function from @n@ bits to @1@ bit
-- NOTE: this is technically unsafe, because we convert the second argument to
-- an 'Int'.
--
-- Example: the truth table for NOT (truth table represented in binary as 10):
--
-- >>> let t = bitVec 1 :: BitVec Int (2^1) in eval t . bitVec <$> [0,1]
-- [1, 0]
eval :: (KnownNat n, Bits t, Integral t) => BitVec t (2 ^ n) -> BitVec t n -> BitVec t 1
eval t = cmpz . testBit t . fromIntegral . unBitVec

-- | The ith partial derivative at x for a function f
-- TODO: @i@ can be larger than n - fix?
partial :: (KnownNat n, Bits t)
  => Int -> (BitVec t n -> BitVec t 1) -> BitVec t n -> BitVec t 1
partial i f x = f x `xor` f (x `xor` e_i)
  where e_i = bit i

-- | @rd1 f@ is the reverse derivative of a function @f : n -> 1@ to its
-- reverse derivative @rf : n + 1 -> n@, a function interpreted as saying,
-- "at a given input and change of output, how should the input change to
-- achieve the given change of output".
-- NOTE: this is represented as a length @n@ tuple of functions, but we haven't
-- encoded this in the type!
-- TODO: better length-encoded list types?
rd1 :: forall n t . (KnownNat n, KnownNat (n+1), Bits t)
  => (BitVec t n -> BitVec t 1) -> [BitVec t (n + 1) -> BitVec t 1]
rd1 f = component f <$> [0 .. n - 1]
  where n = fromIntegral (natVal (Proxy :: Proxy n))

-- | @component f i xy@ computes the @i@th component of the reverse derivative of @f@
-- for a given input @xy@.
component :: (KnownNat n, KnownNat (n+1), Bits t)
  => (BitVec t n -> BitVec t 1) -> Int -> BitVec t (n + 1) -> BitVec t 1
component f i xy = partial i f x .&. y -- df/dx_i * y
  where (x, y) = split xy
