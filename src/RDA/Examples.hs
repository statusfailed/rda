module RDA.Examples
  ( lut4
  , oddDataset
  , bitTestDataset
  , misclassified
  , predictions
  , masking
  , main
  ) where

import Data.Bits
import GHC.TypeNats

import RDA.BitVec
import RDA.ReverseDerivative
import RDA.Optimize

-- | A dataset of 4-bit integers with label = 1 if odd, else 0..
oddDataset :: [(BitVec Int 4, BitVec Int 1)]
oddDataset = (\x -> (bitVec x, y x)) <$> [0..15]
  where y x = if odd x then 1 else 0

bitTestDataset :: [(BitVec Int 4, BitVec Int 1)]
bitTestDataset = (\x -> (bitVec x, if testBit x 3 then 1 else 0)) <$> [0..15]

-- | 'noisyDataset' is just the 'bitTestDataset' with some extra "noisy" labels-
-- x <= 2 will have label 1.
noisyDataset :: [(BitVec Int 4, BitVec Int 1)]
noisyDataset = (\x -> (bitVec x, if testBit x 3 || x <= 2 then 1 else 0)) <$> [0..15]

misclassified :: (Bits t)
  => (BitVec t x -> BitVec t 1) -> [(BitVec t x, BitVec t 1)] -> Int
misclassified model = length . filter (\(x, y) -> model x /= y)

predictions :: Functor f
  => (BitVec t x -> BitVec t 1)
  -> f (BitVec t x, BitVec t 1)
  -> f (t, t, t)
predictions model =
  fmap (\(x, y) -> (unBitVec x, unBitVec y, unBitVec (model x)))

-- a 4-bit LUT model
lut4 :: Bits t => BitVec t (2^4) -> BitVec t 4 -> BitVec t 1
lut4 = eval

-- a simple masking model: returns 1 if any masked bits are on.
masking :: (Num t, Bits t) => BitVec t 4 -> BitVec t 4 -> BitVec t 1
masking params x = if params .&. x /= zeroBits then 1 else 0

-- Test we can learn the "odd" function as a 4-bit LUT.  (indeed, we can: this
-- should terminate in 16 steps with a perfect "classifier")
main :: IO ()
main = do
  --let model = lut4
      --dataset = thresholdDataset
  let model = masking -- try "lut4"
      dataset = noisyDataset -- also try 'bitTestDataset' or 'oddDataset'.

  putStrLn "learning trace..."
  let trace = rda dataset model zeroBits
      final = last trace
  print $ unBitVec <$> trace
  putStrLn $ "final function params: " ++ show final
  putStrLn $ "misclassified items: " ++ show (misclassified (model final) dataset)
  putStrLn "all misclassified: "
  mapM_ print . filter (\(x, y, y') -> y /= y') $ predictions (model final) dataset
