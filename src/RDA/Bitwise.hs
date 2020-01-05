-- | Bitwise operations not otherwise defined in Data.Bits
module RDA.Bitwise where

import Data.Bits

-- | @mask n@ returns a bitvector of @n@ bits set to 1.
-- @mask 4 = 0b1111@
mask :: Bits a => Int -> a
mask n = let z = complement zeroBits in (z `shiftL` n) `xor` z

-- | @cmpz x@ returns 'zeroBits' if x has no bit set, and returns 'bit 0'
-- otherwise.
cmpz :: (Bits t, Bits u) => t -> u
cmpz x
  | x == zeroBits = zeroBits
  | otherwise     = bit 0
