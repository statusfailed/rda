{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Utilities for dealing with type-level naturals
module RDA.Nat (nat) where

import Data.Proxy
import GHC.TypeNats

-- | Utility to quickly grab nat values. Suppose we have a function with a
-- KnownNat type parameter @n@, we can simply use @nat \@n@ within the body of
-- the function to reify @n@.
nat :: forall n t . (KnownNat n, Integral t) => t
nat = (fromIntegral . natVal) (Proxy :: Proxy n)
