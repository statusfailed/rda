-- | Bitwise operations not otherwise defined in Data.Bits
module RDA.Bitwise where

import Data.Bits
import Data.List (findIndex)

-- | @mask n@ returns a bitvector of @n@ bits set to 1.
-- @mask 4 = 0b1111@
mask :: Bits a => Int -> a
mask n = let z = complement zeroBits in (z `shiftL` n) `xor` z

msb :: (Ord t, Bits t) => t -> Int
msb x = maybe (-1) pred $ findIndex (\i -> bit i > x) [0..]

lsb :: (Ord t, Bits t) => t -> Int
lsb x = go 0 x
  where go i x = if x == zeroBits then i else go (succ i) (shiftR x 1)


-- | @cmpz x@ returns 'zeroBits' if x has no bit set, and returns 'bit 0'
-- otherwise.
cmpz :: (Bits t, Bits u) => t -> u
cmpz x
  | x == zeroBits = zeroBits
  | otherwise     = bit 0

-- @n `chunksOf` 2@ splits a @Bits@ value into @n@ @k@-bit "chunks".
-- For example, @3 `chunksOf` 2 $ 63 == [3, 3, 3]@ because @63 = 0b111111@.
chunksOf :: Bits t => Int -> Int -> t -> [t]
chunksOf n k = take n . fmap (mask k .&.) . iterate (\a -> a `shiftR` k)

unsafeConcatBits :: (Bits t, Foldable f) => Int -> f t -> t
unsafeConcatBits n = snd . foldl f (0, zeroBits)
  where f (i, b) a = (i + 1, b `xor` (a `shiftL` (n * i)))
