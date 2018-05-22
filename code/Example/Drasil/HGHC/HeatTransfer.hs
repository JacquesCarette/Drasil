module Drasil.HGHC.HeatTransfer where --whole file is used

import Language.Drasil

import Data.Drasil.Units.Thermodynamics (heat_transfer_coef)

{--}

symbols :: [QuantityDict]
symbols = map qw hghcVars ++ map qw htVars

hghcVars :: [QDefinition]
hghcVars = [htTransCladFuel, htTransCladCool]

htVars :: [VarChunk]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

htInputs, htOutputs :: [QuantityDict]
htInputs = map qw htVars
htOutputs = map qw hghcVars

cladThick, coolFilmCond, gapFilmCond, cladCond :: VarChunk
cladThick    = vc "cladThick"    (cn''' "clad thickness")
  ((Greek Tau_L) `sub` lC) Real
coolFilmCond = vc "coolFilmCond" (cn' "initial coolant film conductance")
  (lH `sub` lB) Real
gapFilmCond  = vc "gapFilmCond"  (cn' "initial gap film conductance")
  (lH `sub` lP) Real
cladCond     = vc "cladCond"     (cnIES "clad conductivity") (lK `sub` lC) Real

htTransCladCool_eq, htTransCladFuel_eq :: Expr
htTransCladCool, htTransCladFuel :: QDefinition

htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP 
  "convective heat transfer coefficient between clad and coolant")
  EmptyS
  (lH `sub` lC) heat_transfer_coef htTransCladCool_eq []

htTransCladCool_eq =
  (2 * (sy cladCond) * (sy coolFilmCond) / (2 * (sy cladCond) + (sy cladThick) 
  * (sy coolFilmCond)))

htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  EmptyS
  (lH `sub` lG) heat_transfer_coef htTransCladFuel_eq []

htTransCladFuel_eq = (2 * (sy cladCond) * (sy gapFilmCond)) / (2 * (sy cladCond)
  + ((sy cladThick) * (sy gapFilmCond)))

hghc :: CommonConcept
hghc = dcc' "hghc" (cn "tiny") "HGHC program" "HGHC"

nuclearPhys, fp :: NamedChunk
nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = nc "fp" (cn "FP")
