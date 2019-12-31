-- | Bitwise operations not otherwise defined in Data.Bits
module RDA.Bitwise where

import Data.Bits

mask :: Bits a => Int -> a
mask n = let z = complement zeroBits in (z `shiftL` n) `xor` z
