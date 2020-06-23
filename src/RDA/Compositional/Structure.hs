module RDA.Compositional.Structure where

import RDA
import Data.Bits
import Control.Arrow ((***))

-- | Tensor product of two models
tensor :: forall a1 a2 b1 b2 . (KnownNat a1, KnownNat a2, KnownNat b1, KnownNat b2)
  => a1 :-> b1
  -> a2 :-> b2
  -> (a1 + a2) :-> (b1 + b2)
tensor (f :-> rf) (g :-> rg) = tensorFwd f g :-> tensorRev rf rg

-- | Run two models on different data, with different parameters.
-- TODO
-- tensorParam :: forall p1 p2 a1 a2 b1 b2
--   . (KnownNat a1, KnownNat a2, KnownNat b1, KnownNat b2)
--   => (p1 + a1) :-> b1
--   -> (p2 + a2) :-> b2
--   -> (a1 + a2) :-> (b1 + b2)
-- tensorParam (f :-> rf) (g :-> rg) = undefined

-- | A combinator to easily run two models in parallel on the same data, but
-- with different parameters.
parallel :: forall pf pg a bf bg
  .  (KnownNat pf, KnownNat pg, KnownNat a, KnownNat bf, KnownNat bg)
  => (pf + a) :-> bf
  -> (pg + a) :-> bg
  -> (pg + pf + a) :-> (bf + bg)
parallel f g = id
  . chain (tensor (identity @(pf + pg)) (copy @a)) 
  . chain (ex @pf @pg @a @a)
  $ tensor @(pf + a) @(pg + a) f g

-- | Turn a model into one that operates on batches of 2 data
batch2 :: forall p a b . (KnownNat p, KnownNat a, KnownNat b)
  => (p + a) :-> b
  -> (p + (a + a)) :-> (b + b)
batch2 f = id
  . chain (tensor (copy @p) (identity @(a + a)))
  . chain (ex @p @p @a @a)
  $ tensor @(p + a) @(p + a) f f

-- 'batch' lets you train a model with multiple examples at the same time.
batch :: forall p n a b . (KnownNat p, KnownNat n, KnownNat a, KnownNat b)
  => ((p + a) :-> b)
  -> ((p + n * a) :-> (n * b))
batch f = id
  . chain (tensor (copyN @p @n) (identity @(n * a)))
  . chain (distribute @n @p @a)
  $ repeated f

-- | parallelN lets you train multiple models with the same data.
-- Is it just the same as twist batch?
parallelN :: forall n p a b . (KnownNat n, KnownNat p, KnownNat a, KnownNat b)
  => (p + a) :-> b
  -> (n * p + a) :-> (b * n)
parallelN f = id
  . chain (tensor (identity @(n * p)) (copyN @a @n))
  . chain (distribute @n @p @a)
  $ repeated f

-- | 'tensor' a morphism @n@ times.
repeated :: forall n a b . (KnownNat n, KnownNat a, KnownNat b)
  => (a :-> b)
  -> ((n * a) :-> (n * b))
repeated (f :-> rf) = repeatedFwd f :-> repeatedRev rf

repeatedFwd :: forall n a b . (KnownNat n, KnownNat a, KnownNat b)
  => (BitVec Integer a -> BitVec Integer b)
  -> BitVec Integer (n * a)
  -> BitVec Integer (n * b)
repeatedFwd f v = concatBits @n @b (f <$> chunks @n @a v)

repeatedRev :: forall n a b . (KnownNat n, KnownNat a, KnownNat b)
  => (BitVec Integer (a + b) -> BitVec Integer a)
  -> BitVec Integer (n * a + n * b)
  -> BitVec Integer (n * a)
repeatedRev rf = repeatedFwd @n @(a+b) @a rf . distributeFwd @n @a @b

-- | 'distribute' is named after distributive law
-- 'distribute' with @n = 2@ is the same as @ex@.
distribute :: forall n p a . (KnownNat n, KnownNat p, KnownNat a)
  => ((n * p) + (n * a)) :-> (n * (p + a))
distribute = distributeFwd @n @p @a :-> distributeRev @n @p @a

distributeFwd :: forall n p a . (KnownNat n, KnownNat p, KnownNat a)
  => BitVec Integer ((n * p) + (n * a))
  -> BitVec Integer (n * (p + a))
distributeFwd v =
  concatBits @n @(p + a) $ zipWith append (chunks @n @p ps) (chunks @n @a as)
  where (ps, as) = split @(n*p) @(n*a) v

distributeRev :: forall n p a . (KnownNat n, KnownNat p, KnownNat a)
  => BitVec Integer (((n * p) + (n * a)) + (n * (p + a))) -- yowza
  -> BitVec Integer ((n * p) + (n * a))
distributeRev v = append (concatBits @n @p ps) (concatBits @n @a as)
  where
    (_, y)   = split @((n*p)+(n*a)) @(n*(p+a)) v
    (ps, as) = unzip (split @p @a <$> chunks @n @(p+a) y)

-------------------------------
-- Cartesian / additive / monoidal structure

-- | blockwise copy
copy :: forall a . KnownNat a => a :-> (a + a)
copy = copyFwd :-> tensorFwd (discardFwd @a) (addFwd @a)

add :: forall n . KnownNat n => (n + n) :-> n
add = addFwd :-> tensorFwd (discardFwd @(n + n)) (copyFwd @n)

discard :: forall n . KnownNat n => n :-> 0
discard = discardFwd :-> tensorFwd (discardFwd @n) (zeroFwd @n)

zero :: forall n . KnownNat n => 0 :-> n
zero = zeroFwd :-> discardFwd @n

-------------------------------
-- Polynomial structure

multiply :: forall a . KnownNat a => (a + a) :-> a
multiply = fwd :-> rev
  where fwd = uncurry (.&.) . split @a . (.&. mask (nat @(a + a)))
                rev v = let (x1, x2, dy) = split3 @a v in append (x2 .&. dy) (x1 .&. dy)

one :: forall n . KnownNat n => 0 :-> n
one = const oneFwd :-> discardFwd @n

-------------------------------
-- Replicated Cartesian / additive operations

-- | Make @n@ blockwise copies of @a@ bits
copyN :: forall a n . (KnownNat a, KnownNat n) => a :-> (n * a)
copyN = copyNFwd :-> copyNRev

addN :: forall a n . (KnownNat a, KnownNat n) => (n * a) :-> a
addN = addNFwd :-> addNRev

copyNFwd :: forall a n . (KnownNat a, KnownNat n)
  => BitVec Integer a
  -> BitVec Integer (a * n)
copyNFwd a = concatBits @n @a (replicate (nat @n) a)

copyNRev :: forall a n . (KnownNat a, KnownNat n)
  => BitVec Integer (a + a * n)
  -> BitVec Integer a
copyNRev = addNFwd @a @n . snd . split @a

addNFwd :: forall a n . (KnownNat a, KnownNat n)
  => BitVec Integer (a * n)
  -> BitVec Integer a
addNFwd = foldl xor zeroBits . chunks @n @a

addNRev :: forall a n . (KnownNat a, KnownNat n)
  => BitVec Integer (a * n + a)
  -> BitVec Integer (a * n)
addNRev = copyNFwd @a @n . snd . split @(a * n)

sum :: forall a . KnownNat a => a :-> 1
sum = parity :-> sumRev

sumRev :: forall a . KnownNat a => BitVec Integer (a + 1) -> BitVec Integer a
sumRev v = if testBit v (nat @a) then mask (nat @a) else zeroBits

-------------------------------

tensorFwd :: forall a1 a2 b1 b2 t
  .  (KnownNat a1, KnownNat a2, KnownNat b1, KnownNat b2, Bits t)
  => (BitVec t a1 -> BitVec t b1)
  -> (BitVec t a2 -> BitVec t b2)
  -> BitVec t (a1 + a2)
  -> BitVec t (b1 + b2)
tensorFwd f g = uncurry append . (f *** g) . split

tensorRev :: forall a1 a2 b1 b2 t
  . (KnownNat a1, KnownNat a2, KnownNat b1, KnownNat b2, Bits t)
  => (BitVec t (a1 + b1) -> BitVec t a1)
  -> (BitVec t (a2 + b2) -> BitVec t a2)
  -> BitVec t (a1 + a2 + b1 + b2)
  -> BitVec t (a1 + a2)
tensorRev rf rg = tensorFwd rf rg . (exFwd @a1 @a2 @b1 @b2)

copyFwd :: forall n t . (KnownNat n, Bits t) => BitVec t n -> BitVec t (n + n)
copyFwd x = append x x

addFwd :: forall n t .  (KnownNat n, Bits t) => BitVec t (n + n) -> BitVec t n
addFwd x = let (a, b) = split @n x in a `xor` b

zeroFwd :: forall n t . (KnownNat n, Bits t) => BitVec t 0 -> BitVec t n
zeroFwd = const zeroBits

discardFwd :: forall n t . (KnownNat n, Bits t) => BitVec t n -> BitVec t 0
discardFwd x = zeroBits

oneFwd :: forall n t . (KnownNat n, Bits t) => BitVec t n
oneFwd = mask (nat @n)

-------------------------------
-- useful combinators

ex :: forall a b c d . (KnownNat a, KnownNat b, KnownNat c, KnownNat d)
  => ((a+b) + (c+d)) :-> ((a+c) + (b+d))
ex = exFwd @a @b @c @d :-> rev
  where
    rev = tensorFwd @(a+b+c+d) @(a+c+b+d) @0 @(a+b+c+d)
                    (discardFwd @(a+b+c+d))
                    (exFwd @a @c @b @d)

swap :: forall a . (KnownNat a) => (a + a) :-> (a + a)
swap = swapFwd @a @a :-> rev
  where rev = tensorFwd @(a+a) @(a+a) @0 @(a+a) (discardFwd @(a+a)) (swapFwd @a @a)

-------------------------------
-- Swaps, ex, identity

exFwd :: forall a b c d t . (KnownNat a, KnownNat b, KnownNat c, KnownNat d, Bits t)
  => BitVec t ((a + b) + (c + d)) -> BitVec t ((a + c) + (b + d))
exFwd = idFwd @a `tensorFwd` (swapFwd @b @c) `tensorFwd` idFwd @d

idFwd :: forall a t . (KnownNat a, Bits t) => BitVec t a -> BitVec t a
idFwd = id

swapFwd :: forall a b t . (KnownNat a, KnownNat b, Bits t)
  => BitVec t (a + b) -> BitVec t (b + a)
swapFwd = uncurry (flip append) . split @a @b

-------------------------------
-- Misc extra primitives

-- | Turn a binary number @n@ into the @n@th basis vector, e.g.,
--
-- >>> run basis i = bit i
basis :: forall a . RDA.KnownNat a => a :-> (2^a)
basis = basisFwd :-> basisRev

basisFwd :: forall a . RDA.KnownNat a => BitVec Integer a -> BitVec Integer (2^a)
basisFwd = bitVec . bit . fromIntegral . unBitVec

basisRev :: forall a . RDA.KnownNat a => BitVec Integer (a + 2^a) -> BitVec Integer a
basisRev v = concatBits @a @1 [cmpz . f . testBit dy . toInt $ complementBit x e_i | e_i <- [0..nat @a]]
  where
        (x, dy) = split @a v
    f = if testBit dy (toInt x) then Prelude.not else id
    toInt = fromIntegral . unBitVec
