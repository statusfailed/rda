module RDA.Eval
  ( eval1
  , eval
  ) where

import Data.Bits
import GHC.TypeNats

import RDA.Nat
import RDA.BitVec
import RDA.Bitwise

-- | Evaluate a truth table of @2^n@ bits as a function from @n@ bits to @1@ bit
-- NOTE: this is technically unsafe for @n > 2^64 - 1@, because we convert the
-- second argument to an 'Int'.
--
-- Example: the truth table for NOT
-- (truth table represented in binary as 0b10, a bitvector of bits [0, 1]):
--
-- >>> let t = bitVec 1 :: BitVec Int (2^1) in eval t . bitVec <$> [0,1]
-- [1, 0]
eval1 :: forall n t . (KnownNat n, Bits t, Integral t)
  => BitVec t (2 ^ n) -> BitVec t n -> BitVec t 1
eval1 t = cmpz . testBit t . fromIntegral . unBitVec

-- | Evaluate a truth table of @b * 2^a@ bits as a function from @a@-dimensional
-- bitvectors to @b@-dimensional bitvectors.
--
-- NOTE: if @b * 2^a > 2^63@ behaviour is undefined.
eval :: forall a b t . (KnownNat a, KnownNat b, Bits t, Integral t)
  => BitVec t (b * (2 ^ a)) -> BitVec t a -> BitVec t b
eval t = resize . shiftR t . (* nat @b) . fromIntegral . unBitVec
