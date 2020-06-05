module Language.Drasil.Uncertainty (defaultUncrt, uncty,
    uncVal, uncPrec, ignoreUncrt) where

import Control.Lens ((^.))

import Language.Drasil.Classes (HasUncertainty(unc))
import Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty)

defaultUncrt :: Uncertainty
defaultUncrt = uncty (Just 0.1) (Just 0)

ignoreUncrt :: Uncertainty
ignoreUncrt = uncty Nothing Nothing

-- accessor for uncertainty value
uncVal :: HasUncertainty x => x -> Double
uncVal u = let 
    extractUncValue :: Maybe Double -> Double
    extractUncValue (Just x) = x
    extractUncValue Nothing = 0.0
    in extractUncValue $ u ^. (unc . uncert)

-- accessor for precision value
uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = u ^. (unc . prec)
