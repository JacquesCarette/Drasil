{-# Language TemplateHaskell #-}
module Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty) where

import Control.Lens (makeLenses)

data Uncertainty = Uncert { _uncert :: Maybe Double, _prec :: Maybe Int }
makeLenses ''Uncertainty

uncty :: Maybe Double -> Maybe Int -> Uncertainty
uncty (Just u) = Uncert (Just $ isDecimal u)
uncty Nothing  = error "To ignore uncertainty please use the ignoreUncert contructor."

--make sure that it is between 0 and 1, and throw an error otherwise
isDecimal :: (Num a, Ord a) => a -> a
isDecimal 0  =  error "To ignore uncertainty please use the ignoreUncert contructor."
isDecimal u  =  if (0 < u) && (u < 1) then u
                else error "Uncertainty must be between 0 and 1"
