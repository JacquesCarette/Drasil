{-# Language TemplateHaskell #-}
module Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty, exact) where

import Control.Lens (makeLenses)

-- | Something that may contain an uncertainty value and a precision value.
data Uncertainty = Uncert { _uncert :: Maybe Double, _prec :: Maybe Int }
makeLenses ''Uncertainty

-- | Smart constructor for values with uncertainty.
uncty :: Double -> Maybe Int -> Uncertainty
uncty u = Uncert (Just $ isDecimal u)

-- | Smart constructor for exact values (no uncertainty).
exact :: Uncertainty
exact = Uncert Nothing Nothing

-- | Make sure that input is between 0 and 1, and throw an error otherwise.
isDecimal :: (Num a, Ord a) => a -> a
isDecimal 0  =  error "An uncertain quantity cannot be exact (have 0% uncertainty). Reconsider whether your value is exact or uncertain"
isDecimal u  =  if (0 < u) && (u < 1) then u
                else error "Uncertainty must be between 0 and 1"
