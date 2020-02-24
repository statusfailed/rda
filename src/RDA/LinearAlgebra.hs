-- | Linear Algebra routines for the 'BitVec' type.
--
-- NOTE: it\'s generally not a good idea to roll your own linear algebra code,
-- and this module is no exception.
{-# LANGUAGE AllowAmbiguousTypes #-} -- ^ needed for type of 'transpose'
module RDA.LinearAlgebra where

import GHC.TypeLits
import Data.Bits

import RDA.Nat
import RDA.BitVec

-- | Multiply a bitvector by a bitmatrix
--
-- Example for the 3x3 identity matrix on 3D vectors:
-- >>> unBitVec . matrixMultiply @3 @3 0x111 <$> [0..7]
-- [0,1,2,3,4,5,6,7]
matrixMultiply :: forall a b t . (KnownNat a, KnownNat b, Bits t)
  => BitVec t (b * a) -- ^ matrix / parameters
  -> BitVec t a       -- ^ vector / input data
  -> BitVec t b       -- ^ output
matrixMultiply p x = concatBits @b . fmap (dotProduct x) $ (chunks @b @a p)

-- | Take the dot product of two vectors over Z_2
dotProduct :: (KnownNat n, Bits t) => BitVec t n -> BitVec t n -> BitVec t 1
dotProduct x y = parity (x .&. y)

-- | NOTE: this type doesn't encode the important part of this operation; namely
-- that it puts a bitvector from 'row-major' form into 'column major' form.
--
-- 0b 001 010 100 111   n = 3, m = 4
-- 0b 0011 0101 1001    transposed
--
-- >>> transpose @2 @3 37 == 49
--
-- TODO: this works but it's probably garbage; basically two nested loops over
-- the bit array- very slow!
transpose :: forall n m t . (Bits t, KnownNat n, KnownNat m)
  => BitVec t (m * n) -- ^ a bitvector representing @m@ @n@-bit parts
  -> BitVec t (n * m) -- ^ a bitvector of @n@ @m@-bit parts
transpose v = foldl xor zeroBits b
  where
    a = chunks @m @n v :: [BitVec t n]

    b = zipWith f [0..] a

    f :: Int -> BitVec t n -> BitVec t (n * m)
    f i x = foldl xor zeroBits $ fmap (g i x) [0..n-1]

    g i x j = if testBit x j then bit (m * j + i) else zeroBits

    n = nat @n
    m = nat @m

