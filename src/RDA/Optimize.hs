{-# LANGUAGE NoStarIsType #-}
module RDA.Optimize
  ( rdaUpdate
  , rda
  ) where

import Data.Bits
import Data.Proxy
import GHC.TypeNats

import RDA.BitVec
import RDA.Eval (eval)
import RDA.ReverseDerivative (rd1)
import RDA.Compositional.Model ((:->)(..))

rdaUpdate :: forall p n m . (KnownNat p, KnownNat n, KnownNat m)
  => ((p + n) :-> m) -- ^ a model
  -> BitVec Integer p -- ^ model parameters
  -> (BitVec Integer n, BitVec Integer m) -- ^ a training example (x, y)
  -> BitVec Integer p -- ^ updated model parameters
rdaUpdate (fwd :-> rev) p (x, y) = p `xor` p'
  where
    px = append p x
    dy = y `xor` fwd px
    (p', _) = split $ rev (append px dy)

rda :: forall p n m . (KnownNat p, KnownNat n, KnownNat m)
  => ((p + n) :-> m) -- ^ a model
  -> BitVec Integer p -- ^ initial model parameters
  -> [(BitVec Integer n, BitVec Integer m)] -- ^ training examples
  -> [BitVec Integer p] -- ^ updated model parameters
rda model = scanl (rdaUpdate model)
