module Language.Drasil.Uncertainty (defaultUncrt, uncty,
    uncVal, uncPrec) where

import Control.Lens ((^.))

import Language.Drasil.Classes (HasUncertainty(unc))
import Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty)

defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)

-- accessor for uncertainty value
uncVal :: HasUncertainty x => x -> Double
uncVal u = (u ^. unc) ^. uncert

-- accessor for precision value
uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = (u ^. unc) ^.  prec
