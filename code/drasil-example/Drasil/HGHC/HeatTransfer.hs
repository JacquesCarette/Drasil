module Drasil.HGHC.HeatTransfer where --whole file is used

import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (DataDefinition, ddNoRefs)

import Data.Drasil.Units.Thermodynamics (heatTransferCoef)

{--}

symbols :: [QuantityDict]
symbols = htOutputs ++ htInputs

dataDefs :: [DataDefinition]
dataDefs = [htTransCladFuelDD, htTransCladCoolDD]

qDefs :: [QDefinition]
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
htTransCladCool, htTransCladFuel :: QDefinition

---

htTransCladCoolDD :: DataDefinition
htTransCladCoolDD = ddNoRefs htTransCladCool Nothing "htTransCladCool"--Label
  []--no additional notes

htTransCladCool = fromEqn "htTransCladCool" (nounPhraseSP
  "convective heat transfer coefficient between clad and coolant")
  EmptyS (sub lH lClad) Real heatTransferCoef htTransCladCoolEq

htTransCladCoolEq =
  2 * sy cladCond * sy coolFilmCond / (2 * sy cladCond + sy cladThick 
  * sy coolFilmCond)

---

htTransCladFuelDD :: DataDefinition
htTransCladFuelDD = ddNoRefs htTransCladFuel Nothing "htTransCladFuel"--Label
  []--no additional notes

htTransCladFuel = fromEqn "htTransCladFuel" (nounPhraseSP
  "effective heat transfer coefficient between clad and fuel surface")
  EmptyS (sub lH lEffective) Real heatTransferCoef htTransCladFuelEq

htTransCladFuelEq = (2 * sy cladCond * sy gapFilmCond) / (2 * sy cladCond
  + (sy cladThick * sy gapFilmCond))

---

hghc :: CommonConcept
hghc = dcc' "hghc" (cn "HGHC") "HGHC program" "HGHC"

nuclearPhys, fp :: NamedChunk
nuclearPhys = nc "nuclearPhys" (nounPhraseSP "nuclear physics")
fp = nc "fp" (cn "FP")

lCoolant, lClad, lEffective, lGap :: Symbol
lCoolant   = Label "b"
lClad      = Label "c"
lEffective = Label "g"
lGap       = Label "p"