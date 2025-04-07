module Drasil.HGHC.HeatTransfer where --whole file is used

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (DataDefinition, ddENoRefs)

import Data.Drasil.Units.Thermodynamics (heatTransferCoef)

{--}

symbols :: [QuantityDict]
symbols = htOutputs ++ htInputs

dataDefs :: [DataDefinition]
dataDefs = [htTransCladFuelDD, htTransCladCoolDD]

qDefs :: [SimpleQDef]
qDefs = [htTransCladFuel, htTransCladCool]

htVars :: [QuantityDict]
htVars = [cladThick, coolFilmCond, gapFilmCond, cladCond]

htInputs, htOutputs :: [QuantityDict]
htInputs = map qw htVars
htOutputs = map qw qDefs

cladThick, coolFilmCond, gapFilmCond, cladCond :: QuantityDict
cladThick    = vc "cladThick"    (cn''' "clad thickness")
  (sub lTau lClad) Real
coolFilmCond = vc "coolFilmCond" (cn' "initial coolant film conductance")
  (sub lH lCoolant) Real
gapFilmCond  = vc "gapFilmCond"  (cn' "initial gap film conductance")
  (sub lH lGap) Real
cladCond     = vc "cladCond"     (cnIES "clad conductivity") (sub lK lClad) Real

htTransCladCoolEq, htTransCladFuelEq :: Expr
htTransCladCool, htTransCladFuel :: SimpleQDef

---

htTransCladCoolDD :: DataDefinition
htTransCladCoolDD = ddENoRefs htTransCladCool Nothing "htTransCladCool"--Label
  []--no additional notes

htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP
  "convective heat transfer coefficient between clad and coolant")
  EmptyS (sub lH lClad) Real heatTransferCoef htTransCladCoolEq

htTransCladCoolEq =
  exactDbl 2 $* sy cladCond $* sy coolFilmCond $/ (exactDbl 2 $* sy cladCond $+ (sy cladThick 
  $* sy coolFilmCond))

---

htTransCladFuelDD :: DataDefinition
htTransCladFuelDD = ddENoRefs htTransCladFuel Nothing "htTransCladFuel"--Label
  []--no additional notes

htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  EmptyS (sub lH lEffective) Real heatTransferCoef htTransCladFuelEq

htTransCladFuelEq = (exactDbl 2 $* sy cladCond $* sy gapFilmCond) $/ (exactDbl 2 $* sy cladCond
  $+ (sy cladThick $* sy gapFilmCond))

---

hghc :: CI
hghc = commonIdea "hghc" (cn "HGHC Toy Example") "HGHC" []

nuclearPhys, fp :: IdeaDict
nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = nc "fp" (cn "FP")

lCoolant, lClad, lEffective, lGap :: Symbol
lCoolant   = label "b"
lClad      = label "c"
lEffective = label "g"
lGap       = label "p"
