module Language.Drasil.Uncertainty.Core (Uncertainty, uncty) where
 
data Uncertainty = Uncert { _uncert :: Double, _prec :: Maybe Int }

uncty :: Double -> Maybe Int -> Uncertainty
uncty u = Uncert (isDecimal u)

--make sure that it is between 0 and 1, and throw an error otherwise
isDecimal :: (Num a, Ord a) => a -> a
isDecimal u  =  if (0 < u) && (u < 1) then u
                else error "Uncertainty must be between 0 and 1."
