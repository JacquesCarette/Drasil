module Language.Drasil.Uncertainty (defaultUncrt, uncty,
    uncVal, uncPrec) where

import Control.Lens ((^.))

import Language.Drasil.Classes (HasUncertainty(uncert, prec))
import Language.Drasil.Uncertainty.Core (Uncertainty, uncty)

defaultUncrt :: Uncertainty
defaultUncrt = uncty 0.1 (Just 0)

-- accessor for uncertainty value
{-
uncVal :: Uncertainty -> Double
uncVal (Uncert val _) = val
-}

uncVal :: HasUncertainty x => x -> Double
uncVal u = u ^. uncert

-- accessor for precision value
{-
uncPrec :: Uncertainty -> Maybe Int
uncPrec (Uncert _ prec) = prec
-}

uncPrec :: HasUncertainty x => x -> Maybe Int
uncPrec u = u ^. prec
