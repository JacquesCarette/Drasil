module Drasil.HGHC.HeatTransfer where

import Language.Drasil

import Data.Drasil.Units.Thermodynamics (heat_transfer_coef)

htVars :: [VarChunk]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

cladThick, coolFilmCond, gapFilmCond, cladCond :: VarChunk
cladThick    = makeVC "cladThick"    "clad thickness"                   ((Greek Tau_L) `sub` lC)
coolFilmCond = makeVC "coolFilmCond" "initial coolant film conductance" (lH `sub` lB)
gapFilmCond  = makeVC "gapFilmCond"  "initial gap film conductance"     (lH `sub` lP)
cladCond     = makeVC "cladCond"     "clad conductivity"                (lK `sub` lC)

htTransCladCool_eq, htTransCladFuel_eq :: Expr
htTransCladCool_eq =
  (2 * (C cladCond) * (C coolFilmCond) / (2 * (C cladCond) + (C cladThick) * (C coolFilmCond)))
htTransCladFuel_eq = (2 * (C cladCond) * (C gapFilmCond)) / (2 * (C cladCond) + ((C cladThick) * (C gapFilmCond)))

htTransCladCool :: QDefinition
htTransCladCool = fromEqn "htTransCladCool" (S 
  "convective heat transfer coefficient between clad and coolant")
  (lH `sub` lC) heat_transfer_coef htTransCladCool_eq

htTransCladFuel :: QDefinition
htTransCladFuel = fromEqn "htTransCladFuel" (S
  "effective heat transfer coefficient between clad and fuel surface")
  (lH `sub` lG) heat_transfer_coef htTransCladFuel_eq
