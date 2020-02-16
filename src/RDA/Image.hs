{-# LANGUAGE AllowAmbiguousTypes #-}
module RDA.Image where

import RDA.Nat
import RDA.Bitwise
import RDA.BitVec
import GHC.TypeNats
import Data.Proxy
import Data.Bits

testImage :: forall t n . (Bits t, KnownNat n)
  => Proxy n -> BitVec t (n * n)
testImage _ = bitVec $ unsafeConcatBits n (bit <$> [0..n-1])
  where n = num (Proxy :: Proxy n)

num :: (Integral a, KnownNat n) => Proxy n -> a
num = fromIntegral . natVal

-- | Safely (except for indexing!) take a sub-image of an image stored in
-- row-major form.
--
-- Example call, with a 2×2 filter kernel over a (2+1)×(2+1) image:
--  @subImage @2 @2 @1 @1 0 0 (273 :: BitVec Integer (3 * 3)) :: BitVec Integer (2 * 2)@
subImage :: forall kw kh w h t
  . (Bits t, KnownNat kw, KnownNat kh, KnownNat w, KnownNat h)
  => Int -- ^ horizontal offset
  -> Int -- ^ vertical   offset
  -> BitVec t ((kw + w) * (kh + h)) -- ^ Input image
  -> BitVec t (kw * kh) -- ^ output image
subImage i j (BitVec x)
  = bitVec $ unsafeSubImage (nat @kw, nat @kh) (nat @w, nat @h) i j x

-- | 2D-Convolve a square filter over a square image in row-major order.
--
-- Example call:
-- @
convolve2D :: forall kw kh w h m t .
  (KnownNat kw, KnownNat kh, KnownNat w, KnownNat h, KnownNat m, Bits t)
  => (BitVec t (kw * kh) -> BitVec t m)   -- ^ A (kw*kh → m) filter kernel
  -> BitVec t ((kw + w) * (kh + h))       -- ^ an image of at least kw*kh dimensions
  -> BitVec t ((w + 1) * (h + 1) * m)     -- ^ A w * h output image of m-bit pixels
convolve2D f =
  bitVec . unsafeConvolve2D (nat @kw, nat @kh) (nat @w, nat @h) (nat @m) f' . unBitVec
  where
    f' = unBitVec . f . bitVec :: t -> t

-- | @unsafeConvolve2D (kw, kh) (w, h) m f x@ applies the filter kernel @f@ to
-- each @kw × kh@ patch of the @w × h@ image stored in the bitvector @x@.
-- Note that no dimensions are checked, and the image will be padded with zeros
-- if it is smaller than the specified dimensions.
unsafeConvolve2D :: (Bits t)
  => (Int, Int) -- ^ @(kw, kh)@ kernel size (w, h)
  -> (Int, Int) -- ^ @(w, h)@ image size (as delta from kernel size) (w, h)
  -> Int        -- ^ @m@ kernel output size (# output bits)
  -> (t -> t)   -- ^ @f@, a function of type @kw*kh → m@
  -> t          -- ^ Input image, row-major form.
  -> t          -- ^ Output image, row-major form, @m@-bit pixels.
unsafeConvolve2D k@(kw, kh) d@(w, h) m f x = unsafeConcatBits m ys
  where
    -- NOTE: TODO: shouldn't this be w + 1, not kw-1?
    ys = [ f (unsafeSubImage k d i j x) | i <- [0..kw-1], j <- [0..kh-1] ]

-- | Unsafely slice a 2-dimensional image stored in row-major form in an 'Integer'
-- and return a subimage of given size.
unsafeSubImage :: Bits t => (Int, Int) -> (Int, Int) -> Int -> Int -> t -> t
unsafeSubImage (kw, kh) (w, h) i j x
  = unsafeConcatBits kw
  . fmap (mask kw .&.)
  . (kh `chunksOf` (kw+w))
  $ shiftR x (j * (kw + w) + i)

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

    n = num (Proxy :: Proxy n) :: Int
    m = num (Proxy :: Proxy m) :: Int

-- | Tumble a @kw × kh@ window across an image of size @(kw × w) × (kh × h)@,
-- to produce an image of size @(w × h)@.
unsafeTumble2D :: (Bits t)
  => (Int, Int) -- ^ @(kw, kh)@ kernel size (w, h)
  -> (Int, Int) -- ^ @(w, h)@ image size (as multiple of kernel size) (w, h)
  -> Int        -- ^ @m@ kernel output size (# output bits)
  -> (t -> t)   -- ^ @f@, a function of type @kw*kh → m@
  -> t          -- ^ Input image, row-major form.
  -> t          -- ^ Output image, row-major form, @m@-bit pixels.
unsafeTumble2D k@(kw, kh) d@(w, h) m f x = unsafeConcatBits m ys
  where
    ys = [ f (unsafeSubImage k d (kw*i) (kh*j) x) | i <- [0..w-1], j <- [0..h-1] ]
