-- | Type-safe bitvectors
-- Thanks: https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html
module RDA.BitVec
  ( BitVec(..) -- ^ TODO: consider hiding constructors
  , bitVec
  , toBits
  , convert
  , append
  , split
  ) where

import Data.Proxy
import GHC.TypeNats

import Data.Bits
import RDA.Bitwise (mask)

-- | A @'BitVec' n a@ is an @n@-bit vector stored as the type a.
newtype BitVec t (n :: Nat) = BitVec { unBitVec :: t }
  deriving(Eq, Ord, Read, Show, Enum, Num, Bits)

instance (KnownNat n, Bits t) => FiniteBits (BitVec t n) where
  finiteBitSize b = fromIntegral (natVal b)

-- | @bitVec n x@ makes a bitvector from a size @n@ and an underlying value @x@,
-- which is taken modulo @2^n@.
--
-- NOTE: using cheeky recursion, we don't have to supply a proxy here, so the
-- value can be polymorphic in length.
--
-- :t bitVec 3 :: BitVec Integer 5
-- BitVec Integer 5
bitVec :: (KnownNat n, Bits t) => t -> BitVec t n
bitVec x = r
  where
    r = BitVec (mask n .&. x)
    n = fromIntegral (natVal r)

-- | Get the underlying representation of a 'BitVec'
toBits :: (KnownNat n, Bits t) => BitVec t n -> t
toBits b@(BitVec x) = mask n .&. x
  where n = fromIntegral (natVal b) -- using bitvec as its own proxy.

-- | Arbitrarily resize a bitvector, discarding most significant bits if m < n
resize :: (KnownNat n, Bits t) => BitVec t n -> BitVec t m
resize (BitVec x) = BitVec x

-- | Change the underlying representation of a 'BitVec'
-- /O(n)/.
convert :: (KnownNat n, Bits a, Bits b) => BitVec a n -> BitVec b n
convert a =
  foldl (\b i -> if testBit a i then setBit b i else b) zeroBits [0..finiteBitSize a]

-- | Enlarge a vector
expand :: (Bits t, KnownNat n, KnownNat m, n <= m) => BitVec t n -> BitVec t m
expand (BitVec x) = BitVec x

-- | Append two 'BitVec's.
append :: (Bits t, KnownNat n, KnownNat m, m <= n + m, n <= n + m, KnownNat (n + m))
  => BitVec t n -> BitVec t m -> BitVec t (n + m)
append x y = expand x `xor` (expand y `shiftL` n) -- shiftL: we write numbers backwards!
  where n = fromIntegral $ natVal x

split :: (KnownNat n, KnownNat m, KnownNat (n + m), Bits t)
  => BitVec t (n + m) -> (BitVec t n, BitVec t m)
split xy = (x, y)
  where
    x = resize xy
    y = resize (xy `shiftR` m)
    m = fromIntegral (natVal y)
