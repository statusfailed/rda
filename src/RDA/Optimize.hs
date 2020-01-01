{-# LANGUAGE NoStarIsType #-}
module RDA.Optimize
  ( rdaUpdate
  , rda

  -- debugging / stuff to (re)move
  , lut4
  , oddDataset
  , thresholdDataset
  , bitTestDataset
  , misclassified
  , predictions
  , masking
  , main
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
  => [(BitVec t x, BitVec t 1)]
  -> (BitVec t p -> BitVec t x -> BitVec t 1)
  -> BitVec t p
  -> [BitVec t p]
rda [] _ params = [params]
rda ((x, y):dataset) model params = params : rda dataset model params'
  where
    params' = params `xor` rdaUpdate params x y model

oddDataset :: [(BitVec Int 4, BitVec Int 1)]
oddDataset = (\x -> (bitVec x, y x)) <$> [0..15]
  where y x = if odd x then 1 else 0

thresholdDataset :: [(BitVec Int 4, BitVec Int 1)]
thresholdDataset = (\x -> (bitVec x, y x)) <$> [0..15]
  where y x = if x `testBit` 3 then 1 else 0

bitTestDataset :: [(BitVec Int 4, BitVec Int 1)]
bitTestDataset = (\x -> (bitVec x, if testBit x 3 then 1 else 0)) <$> [0..15]

misclassified model = length . filter (\(x, y) -> model x /= y)

predictions model = fmap (\(x, y) -> (unBitVec x, unBitVec y, unBitVec (model x)))

-- a 4-bit LUT
lut4 :: Bits t => BitVec t (2^4) -> BitVec t 4 -> BitVec t 1
lut4 = eval

-- a simple masking model
masking :: (Num t, Bits t) => BitVec t 4 -> BitVec t 4 -> BitVec t 1
masking params x = if params .&. x /= zeroBits then 1 else 0

-- Test we can learn the "odd" function as a 4-bit LUT.
-- (indeed, we can: this should terminate in 16 steps with a perfect "classifier")
main :: IO ()
main = do
  --let model = lut4
      --dataset = thresholdDataset
  let model = masking
      dataset = bitTestDataset

  putStrLn "learning trace..."
  let trace = rda dataset model zeroBits
      final = last trace
  mapM_ print trace
  putStrLn $ "final function: " ++ show final
  putStrLn $ "misclassified items: " ++ show (misclassified (model final) dataset)
  putStrLn "all misclassified: "
  mapM_ print . filter (\(x, y, y') -> y /= y') $ predictions (model final) dataset
