module Language.Drasil.Uncertainty (defaultUncrt, uncty,
    uncVal, uncPrec, exact) where

import Control.Lens ((^.))

import Language.Drasil.Classes (HasUncertainty(unc))
import Language.Drasil.Uncertainty.Core (Uncertainty, uncert, prec, uncty, exact)
import Data.Maybe (fromMaybe)

-- | The default uncertainty is set to 0.1.
defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)

-- | Accessor for uncertainty value from something that has an uncertainty.
uncVal :: HasUncertainty x => x -> Double
uncVal u = fromMaybe 0.0 $ u ^. (unc . uncert)

-- | Accessor for precision value from something that has an uncertainty.
uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = u ^. (unc . prec)
