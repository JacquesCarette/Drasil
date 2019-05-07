{-# Language TemplateHaskell #-}
module Language.Drasil.Uncertainty (Uncertainty, defaultUncrt, uncty) where

import Control.Lens (makeLenses)

import Language.Drasil.Classes (HasUncertainty(valUnc, prcUnc))
 
data Uncertainty = Uncert { _uncert :: Double, _prec :: Maybe Int }
makeLenses ''Uncertainty

instance HasUncertainty Uncertainty where
    valUnc = uncert
    prcUnc = prec

--make sure that it is between 0 and 1, and throw an error otherwise
isDecimal :: (Num a, Ord a) => a -> a
isDecimal u  =  if (0 < u) && (u < 1) then u
                else error "Uncertainty must be between 0 and 1."

uncty :: Double -> Maybe Int -> Uncertainty
uncty u = Uncert (isDecimal u)

defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)
