-- | Assigns symbols and units (quantities) to thermodynamics-related concepts.
module Data.Drasil.Quantities.Thermodynamics where

import Language.Drasil
import Language.Drasil.ShortHands (cT, cC, lQ, cQ, cE)
import Drasil.Database (mkUid)

import Data.Drasil.SI_Units (centigrade, joule)
import qualified Data.Drasil.Concepts.Thermodynamics as CT (heatCapSpec, htFlux)
import qualified Data.Drasil.Units.Thermodynamics as UT (heatCapSpec, thermalFlux)

-- * With Units

thermoquants :: [DefinedQuantityDict]
thermoquants = [boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat]

boilPt, temp, heatCapSpec, htFlux, latentHeat, meltPt, sensHeat :: DefinedQuantityDict

boilPt        = quant (mkUid "boilPt") (cn' "boiling point temperature")
                  (S "the temperature at which a substance changes from liquid to vapour")
                  (sub cT (label "boil")) Real centigrade
temp          = quant (mkUid "temperature") (cn' "temperature")
                  (S "the degree or intensity of heat present in a substance or object")
                  cT Real centigrade
heatCapSpec   = dqd CT.heatCapSpec cC                      Real UT.heatCapSpec
htFlux        = dqd CT.htFlux      lQ                      Real UT.thermalFlux
latentHeat    = quant (mkUid "latentHeat") (cn' "latent heat")
                  (S ("the heat required to convert a solid into a liquid or " <>
                      "vapor, or a liquid into a vapor, without change of temperature"))
                  cQ Real joule
meltPt        = quant (mkUid "meltPt") (cn' "melting point temperature")
                  (S "the temperature at which a substance changes from solid to liquid")
                  (sub cT (label "melt")) Real centigrade
sensHeat      = quant (mkUid "sensHeat") (cn' "sensible heat")
                  (S ("heat exchanged by a body in which the exchange of heat " <>
                      "changes the temperature and some macroscopic variables of " <>
                      "the body or system, but leaves others unchanged"))
                  cE Real joule
