module Language.Drasil.Uncertainty
  (Uncertainty, uncty) where
 
data Uncertainty = Uncert { _uncert :: Double, _prec :: Maybe Integer }

--make sure that it is between 0 and 1, and throw an error otherwise
isDecimal :: (Num a, Ord a) => a -> a
isDecimal u  =  if (0 < u) && (u < 1) then u
                else error "Uncertainty must be between 0 and 1."

uncty :: Double -> Maybe Integer -> Uncertainty
uncty u = Uncert (isDecimal u)
