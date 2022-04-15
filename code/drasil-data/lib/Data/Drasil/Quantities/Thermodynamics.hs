-- | Assigns symbols and units (quantities) to thermodynamics-related concepts.
module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Language.Drasil.ShortHands (cT, cC, lQ, cQ, cE)

import Data.Drasil.Concepts.Thermodynamics as CT (boilPt, heatCapSpec,
  htFlux, latentHeat, meltPt, sensHeat, temp)
import Data.Drasil.SI_Units (centigrade, joule)
import Data.Drasil.Units.Thermodynamics as UT (heatCapSpec, thermalFlux)

-- * With Units

boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat :: UnitalChunk

boilPt        = uc CT.boilPt (sub cT (label "boil")) centigrade
temp          = uc CT.temp cT centigrade
heatCapSpec   = uc CT.heatCapSpec cC UT.heatCapSpec
htFlux        = uc CT.htFlux lQ UT.thermalFlux
latentHeat    = uc CT.latentHeat cQ joule
meltPt        = uc CT.meltPt (sub cT (label "melt")) centigrade
sensHeat      = uc CT.sensHeat cE joule
