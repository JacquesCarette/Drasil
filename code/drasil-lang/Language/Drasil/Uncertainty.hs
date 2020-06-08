module Language.Drasil.Uncertainty (defaultUncrt, uncty,
    uncVal, uncPrec, exact) where

import Control.Lens ((^.))

import Language.Drasil.Classes (HasUncertainty(unc))
import Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty, exact)

defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)

-- accessor for uncertainty value
uncVal :: HasUncertainty x => x -> Double
uncVal u = maybe 0.0 id $ u ^. (unc . uncert)


-- accessor for precision value
uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = u ^. (unc . prec)
