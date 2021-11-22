{-# Language TemplateHaskell #-}
-- | Defines uncertainty types and functions.
module Language.Drasil.Uncertainty (
  -- * Type
  Uncertainty,
  -- * Class
  HasUncertainty(..),
  -- * Lenses
  uncert, prec,
  -- * Constructors
  uncty, exact,
  -- * Constructor
  defaultUncrt,
  -- * Accessors
  uncVal, uncPrec,
) where

import Control.Lens (Lens', (^.), makeLenses)

import Data.Maybe (fromMaybe)

-- | Something that may contain an uncertainty value and a precision value.
data Uncertainty = Uncert {
  _uncert :: Maybe Double,
  _prec   :: Maybe Int
}
makeLenses ''Uncertainty

-- | HasUncertainty is just a chunk with some uncertainty associated to it.
-- This uncertainty is represented as a decimal value between 0 and 1 (percentage).
class HasUncertainty c where
  -- | Provides the 'Lens' to an 'Uncertainty'.
  unc  :: Lens' c Uncertainty

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

-- | The default uncertainty is set to 0.1.
defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)

-- | Accessor for uncertainty value from something that has an uncertainty.
uncVal :: HasUncertainty x => x -> Double
uncVal u = fromMaybe 0.0 $ u ^. (unc . uncert)

-- | Accessor for precision value from something that has an uncertainty.
uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = u ^. (unc . prec)
