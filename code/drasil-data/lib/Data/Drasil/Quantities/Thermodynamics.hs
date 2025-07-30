-- | Assigns symbols and units (quantities) to thermodynamics-related concepts.
module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Language.Drasil.ShortHands (cT, cC, lQ, cQ, cE)

import Data.Drasil.SI_Units (centigrade, joule)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (boilPt, heatCapSpec,
  htFlux, latentHeat, meltPt, sensHeat, temp)
import qualified Data.Drasil.Units.Thermodynamics as UT (heatCapSpec, thermalFlux)

-- * With Units

thermoquants :: [UnitalChunk]
thermoquants = [boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat]

boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat :: UnitalChunk

boilPt        = uc CT.boilPt      (sub cT (label "boil")) Real centigrade
temp          = uc CT.temp        cT                      Real centigrade
heatCapSpec   = uc CT.heatCapSpec cC                      Real UT.heatCapSpec
htFlux        = uc CT.htFlux      lQ                      Real UT.thermalFlux
latentHeat    = uc CT.latentHeat  cQ                      Real joule
meltPt        = uc CT.meltPt      (sub cT (label "melt")) Real centigrade
sensHeat      = uc CT.sensHeat    cE                      Real joule
