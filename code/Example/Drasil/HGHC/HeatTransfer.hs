module Drasil.HGHC.HeatTransfer where

import Language.Drasil

import Data.Drasil.Units.Thermodynamics (heat_transfer_coef)

hghcSymMap :: SymbolMap
hghcSymMap = symbolMap symbols

symbols :: [QSWrapper]
symbols = map qs vars ++ map qs htVars

vars :: [QDefinition]
vars = [htTransCladFuel, htTransCladCool]

htVars :: [VarChunk]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

cladThick, coolFilmCond, gapFilmCond, cladCond :: VarChunk
cladThick    = makeVC "cladThick"    (cn''' "clad thickness")
  ((Greek Tau_L) `sub` lC)
coolFilmCond = makeVC "coolFilmCond" (cn' "initial coolant film conductance")
  (lH `sub` lB)
gapFilmCond  = makeVC "gapFilmCond"  (cn' "initial gap film conductance")
  (lH `sub` lP)
cladCond     = makeVC "cladCond"     (cnIES "clad conductivity") (lK `sub` lC)

htTransCladCool_eq :: Expr
htTransCladCool_eq =
  (2 * (C cladCond) * (C coolFilmCond) / (2 * (C cladCond) + (C cladThick) * (C coolFilmCond)))

htTransCladFuel_eq :: Expr
htTransCladFuel_eq = (2 * (C cladCond) * (C gapFilmCond)) / (2 * (C cladCond) + ((C cladThick) * (C gapFilmCond)))

htTransCladCool :: QDefinition
htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP 
  "convective heat transfer coefficient between clad and coolant")
  (lH `sub` lC) heat_transfer_coef htTransCladCool_eq

htTransCladFuel :: QDefinition
htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  (lH `sub` lG) heat_transfer_coef htTransCladFuel_eq

hghc, nuclearPhys, fp :: NPNC
hghc = npnc "hghc" (cn "tiny")
nuclearPhys = npnc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = npnc "fp" (cn "FP")

