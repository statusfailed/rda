{-# LANGUAGE AllowAmbiguousTypes #-}
module RDA.Compositional.Model where

import Prelude hiding (id, (.))
import Control.Category
import GHC.TypeNats
import Data.Kind
import Data.Bits

import RDA.BitVec

-- | The ':->' type is an analogue of dual numbers as used in automatic
-- differentiation.
--
-- Technically, it's the /dual of the linear fibration/ as described in
-- https://arxiv.org/pdf/1910.07065.pdf (see proposition 31)
data n :-> m where
  (:->) :: (KnownNat n, KnownNat m)
    => (BitVec Integer n       -> BitVec Integer m)
    -> (BitVec Integer (n + m) -> BitVec Integer n)
    -> (n :-> m)

-- | Run a model forward
run :: (a :-> b) -> (BitVec Integer a -> BitVec Integer b)
run (f :-> _) = f

-- | Run the reverse derivative of a model
--
-- NOTE: for a compositionally-constructed model, this is not the reverse
-- derivative of a boolean circuit unless the forward circuit is *safe* (see
-- paper)
runRev :: (a :-> b) -> BitVec Integer (a + b) -> BitVec Integer a
runRev (_ :-> rf) = rf

identity :: forall n . KnownNat n => n :-> n
identity = id :-> (snd . split)

-- the chain rule (composition!)
chain :: forall a b c . (KnownNat a, KnownNat b, KnownNat c)
  => (a :-> b) -> (b :-> c) -> (a :-> c) 
chain (f :-> rf) (g :-> rg) = (g . f) :-> rfg
  where
    rfg v = let (a, c) = split @a @c v in rf (append a $ rg (append (f a) c))

-- | Chain for /parametrised/ functions
-- we are really composing (see notes dated 2020-02-12):
--   f : p_f + a → b
--   g : p_g + b → c
-- by splitting the parameters:
--   (id × f) g : p_g + (p_f + a) → c
chainParam :: forall pf pg a b c
  .  (KnownNat pf, KnownNat pg, KnownNat a, KnownNat b, KnownNat c)
  => ((pf + a) :-> b) -> ((pg + b) :-> c) -> (((pg + pf) + a) :-> c) 
chainParam (f :-> rf) (g :-> rg) = fwd :-> rev
  where
    fwd :: BitVec Integer ((pg + pf) + a) -> BitVec Integer c
    fwd p =
      let (pg, pf, a) = split3 @pg @pf @a p
      in  g . append pg . f $ append pf a

    -- yeesh. this is kinda nasty. might be better if we didn't have to append +
    -- split bitvectors everywhere- might as well just use a tuple HList, but
    -- then we'd lose monoidal strictness(?)
    rev :: BitVec Integer (((pg + pf) + a) + c) -> BitVec Integer ((pg + pf) + a)
    rev pac =
      let (pa, c)      = split pac
          (pg, pf, a)  = split3 @pg @pf @a pa
          b            = f (append pf a)
          (pg', b')    = split @pg @b . rg $ append (append pg b) c
          (pf', a')    = split @pf @a . rf $ append (append pf a) b'
      in  append (append pg' pf') a'
