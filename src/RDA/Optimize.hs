{-# LANGUAGE NoStarIsType #-}
module RDA.Optimize
  ( rdaUpdate
  , rda
  ) where

import Data.Bits
import Data.Proxy
import GHC.TypeNats

import RDA.BitVec
import RDA.ReverseDerivative (rd1, eval)

-- | A single update of the RDA algorithm
rdaUpdate :: forall t p x . (KnownNat p, KnownNat (p+1), p <= p + 1, 1 <= p + 1, KnownNat x, Bits t)
  => BitVec t p 
  -> BitVec t x 
  -> BitVec t 1
  -> (BitVec t p -> BitVec t x -> BitVec t 1)
  -> BitVec t p
rdaUpdate params x y model = concatBits (Proxy :: Proxy p) dw
  where
    y' = model params x -- fix (params, x) and evaluate
    dy = y `xor` y' :: BitVec t 1
    fs = rd1 (\p -> model p x) :: [ BitVec t (p + 1) -> BitVec t 1 ]
    dw = reverse $ fmap ($ append params dy) fs -- NOTE: reverse very important(!)

rda :: (KnownNat p, KnownNat x, KnownNat (p+1), Bits t, p <= p + 1, 1 <= p + 1)
  => [(BitVec t x, BitVec t 1)]  -- ^ A dataset of examples to learn
  -> (BitVec t p -> BitVec t x -> BitVec t 1) -- ^ A model function from parameters and data to label
  -> BitVec t p -- ^ Initial parameters (where to begin the search)
  -> [BitVec t p] -- ^ Parameter search trace
rda [] _ params = [params]
rda ((x, y):dataset) model params = params : rda dataset model params'
  where
    params' = params `xor` rdaUpdate params x y model
