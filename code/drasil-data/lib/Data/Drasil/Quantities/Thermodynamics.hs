-- | Assigns symbols and units (quantities) to thermodynamics-related concepts.
module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Language.Drasil.ShortHands (cT, cC, lQ, cQ, cE)

import Data.Drasil.SI_Units (centigrade, joule)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (boilPt, heatCapSpec,
  htFlux, latentHeat, meltPt, sensHeat, temp)
import qualified Data.Drasil.Units.Thermodynamics as UT (heatCapSpec, thermalFlux)

-- * With Units

thermoquants :: [DefinedQuantityDict]
thermoquants = [boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat]

boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat :: DefinedQuantityDict

boilPt        = dqd CT.boilPt      (sub cT (label "boil")) Real centigrade
temp          = dqd CT.temp        cT                      Real centigrade
heatCapSpec   = dqd CT.heatCapSpec cC                      Real UT.heatCapSpec
htFlux        = dqd CT.htFlux      lQ                      Real UT.thermalFlux
latentHeat    = dqd CT.latentHeat  cQ                      Real joule
meltPt        = dqd CT.meltPt      (sub cT (label "melt")) Real centigrade
sensHeat      = dqd CT.sensHeat    cE                      Real joule
