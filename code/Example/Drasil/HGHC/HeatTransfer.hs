module Drasil.HGHC.HeatTransfer where --whole file is used

import Language.Drasil

import Data.Drasil.Units.Thermodynamics (heat_transfer_coef)

{--}

symbols :: [QWrapper]
symbols = map qs hghcVars ++ map qs htVars

hghcVars :: [QDefinition]
hghcVars = [htTransCladFuel, htTransCladCool]

htVars :: [VarChunk]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

htInputs, htOutputs :: [QWrapper]
htInputs = map qs htVars
htOutputs = map qs hghcVars

cladThick, coolFilmCond, gapFilmCond, cladCond :: VarChunk
cladThick    = makeVC "cladThick"    (cn''' "clad thickness")
  ((Greek Tau_L) `sub` lC)
coolFilmCond = makeVC "coolFilmCond" (cn' "initial coolant film conductance")
  (lH `sub` lB)
gapFilmCond  = makeVC "gapFilmCond"  (cn' "initial gap film conductance")
  (lH `sub` lP)
cladCond     = makeVC "cladCond"     (cnIES "clad conductivity") (lK `sub` lC)

htTransCladCool_eq, htTransCladFuel_eq :: Expr
htTransCladCool, htTransCladFuel :: QDefinition

htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP 
  "convective heat transfer coefficient between clad and coolant")
  EmptyS
  (lH `sub` lC) heat_transfer_coef htTransCladCool_eq

htTransCladCool_eq =
  (2 * (C cladCond) * (C coolFilmCond) / (2 * (C cladCond) + (C cladThick) 
  * (C coolFilmCond)))

htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  EmptyS
  (lH `sub` lG) heat_transfer_coef htTransCladFuel_eq

htTransCladFuel_eq = (2 * (C cladCond) * (C gapFilmCond)) / (2 * (C cladCond)
  + ((C cladThick) * (C gapFilmCond)))

hghc, nuclearPhys, fp :: NamedChunk
hghc = nc "hghc" (cn "tiny")
nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = nc "fp" (cn "FP")
