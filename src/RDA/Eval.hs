module RDA.Eval
  ( eval
  ) where

import Data.Bits
import GHC.TypeNats

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
eval :: forall n t . (KnownNat n, Bits t, Integral t)
  => BitVec t (2 ^ n) -> BitVec t n -> BitVec t 1
eval t = cmpz . testBit t . fromIntegral . unBitVec
