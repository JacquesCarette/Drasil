module Drasil.HGHC.HeatTransfer (module Drasil.HGHC.HeatTransfer) where --whole file is used

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

import Drasil.Database (mkUid)
import Language.Drasil
import Language.Drasil.ShortHands
import Theory.Drasil (DataDefinition, ddENoRefs)

import Data.Drasil.Units.Thermodynamics (heatTransferCoef)

symbols :: [DefinedQuantityDict]
symbols = NE.toList htOutputs ++ NE.toList htInputs

dataDefs :: [DataDefinition]
dataDefs = [htTransCladFuelDD, htTransCladCoolDD]

qDefs :: NE.NonEmpty SimpleQDef
qDefs = htTransCladFuel :| [htTransCladCool]

htVars :: NE.NonEmpty DefinedQuantityDict
htVars = cladThick :| [coolFilmCond, gapFilmCond, cladCond]

htInputs, htOutputs :: NE.NonEmpty DefinedQuantityDict
htInputs = NE.map dqdWr htVars
htOutputs = NE.map dqdWr qDefs

cladThick, coolFilmCond, gapFilmCond, cladCond :: DefinedQuantityDict
cladThick    = dqdNoUnit (cncpt''' (mkUid "cladThick")    (cn''' "clad thickness")
  (S "the clad thickness"))
  (sub lTau lClad) Real
coolFilmCond = dqdNoUnit (cncpt''' (mkUid "coolFilmCond") (cn' "initial coolant film conductance")
  (S "the initial coolant film conductance"))
  (sub lH lCoolant) Real
gapFilmCond  = dqdNoUnit (cncpt''' (mkUid "gapFilmCond")  (cn' "initial gap film conductance")
  (S "the initial gap film conductance"))
  (sub lH lGap) Real
cladCond     = dqdNoUnit (cncpt''' (mkUid "cladCond")     (cnIES "clad conductivity")
  (S "the clad conductivity"))
  (sub lK lClad) Real

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

nuclearPhys, fp :: IdeaDict
nuclearPhys = idea' (mkUid "nuclearPhys") (nounPhraseSP "nuclear physics")
fp = idea' (mkUid "fp") (cn "FP")

lCoolant, lClad, lEffective, lGap :: Symbol
lCoolant   = label "b"
lClad      = label "c"
lEffective = label "g"
lGap       = label "p"
