module Drasil.HGHC.HeatTransfer where --whole file is used

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.Units.Thermodynamics (heatTransferCoef)

{--}

symbols :: [QuantityDict]
symbols = htOutputs ++ htInputs

hghcVarsDD :: [DataDefinition]
hghcVarsDD = [htTransCladFuelDD, htTransCladCoolDD]

hghcVars :: [QDefinition]
hghcVars = [htTransCladFuel, htTransCladCool]

htVars :: [QuantityDict]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

htInputs, htOutputs :: [QuantityDict]
htInputs = map qw htVars
htOutputs = map qw hghcVars

cladThick, coolFilmCond, gapFilmCond, cladCond :: QuantityDict
cladThick    = vc "cladThick"    (cn''' "clad thickness")
  (lTau `sub` lC) Real
coolFilmCond = vc "coolFilmCond" (cn' "initial coolant film conductance")
  (lH `sub` lB) Real
gapFilmCond  = vc "gapFilmCond"  (cn' "initial gap film conductance")
  (lH `sub` lP) Real
cladCond     = vc "cladCond"     (cnIES "clad conductivity") (lK `sub` lC) Real

htTransCladCoolEq, htTransCladFuelEq :: Expr
htTransCladCool, htTransCladFuel :: QDefinition

---

htTransCladCoolDD :: DataDefinition
htTransCladCoolDD = mkDD htTransCladCool [{-References-}] [{-Derivation-}] "htTransCladCool"--Label
  []--no additional notes

htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP
  "convective heat transfer coefficient between clad and coolant")
  EmptyS
  (lH `sub` lC) heatTransferCoef htTransCladCoolEq

htTransCladCoolEq =
  (2 * (sy cladCond) * (sy coolFilmCond) / (2 * (sy cladCond) + (sy cladThick) 
  * (sy coolFilmCond)))

---

htTransCladFuelDD :: DataDefinition
htTransCladFuelDD = mkDD htTransCladFuel [{-References-}] [{-Derivation-}] "htTransCladFuel"--Label
  []--no additional notes

htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  EmptyS
  (lH `sub` lG) heatTransferCoef htTransCladFuelEq

htTransCladFuelEq = (2 * (sy cladCond) * (sy gapFilmCond)) / (2 * (sy cladCond)
  + ((sy cladThick) * (sy gapFilmCond)))

---

hghc :: CommonConcept
hghc = dcc' "hghc" (cn "tiny") "HGHC program" "HGHC"

nuclearPhys, fp :: NamedChunk
nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = nc "fp" (cn "FP")
