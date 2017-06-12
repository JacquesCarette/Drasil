module Drasil.SSP.DataDefs where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Drasil.SSP.Units
import Data.Drasil.SI_Units

------------------------
--  Data Definitions  --
------------------------

sspDataDefs :: [QDefinition]
sspDataDefs = [sliceWght, baseWtrF, surfWtrF, intersliceWtrF, angles,
  lengths, seismicLoadF, surfLoads, intrsliceF, mobShearWO, resShearWO,
  mobShearWO, displcmntRxnF, netFDsplcmntEqbm, soilStiffness]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"

--DD1

sliceWght :: QDefinition
sliceWght = fromEqn (wi ^. id) (wi ^. term) (wi ^. symbol) newton slcWgtEqn

slcWgtEqn :: Expr
slcWgtEqn = (Int 0) --FIXME: add the long equation

--DD2

baseWtrF :: QDefinition
baseWtrF = fromEqn' (ubi ^. id) (ubi ^. term) (ubi ^. symbol) bsWtrFEqn 

bsWtrFEqn :: Expr
bsWtrFEqn = (Int 0)

--DD3

surfWtrF :: QDefinition
surfWtrF = fromEqn' (uti ^. id) (uti ^. term) (uti ^. symbol) surfWtrFEqn

surfWtrFEqn :: Expr
surfWtrFEqn = (Int 0)

--DD4

intersliceWtrF :: QDefinition
intersliceWtrF = fromEqn' (hi ^. id) (hi ^. term) (hi ^. symbol) intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = (Int 0)

--DD5

angles :: QDefinition
angles = fromEqn' (baseAngle ^. id) (baseAngle ^. term) (baseAngle ^. symbol) anglesEqn --, beta_i?

anglesEqn :: Expr
anglesEqn = (Int 0)

--DD6

lengths :: QDefinition
lengths = fromEqn' (lbi ^. id) (lbi ^. term) (lbi ^. symbol) lengthsEqn --, lsi?, bi?

lengthsEqn :: Expr
lengthsEqn = (Int 0)

--DD7

seismicLoadF :: QDefinition
seismicLoadF = fromEqn' (kc ^. id) (kc ^. term) (kc ^. symbol) ssmcLFEqn --correct chunk referenced for definition?

ssmcLFEqn :: Expr
ssmcLFEqn = (Int 0)

--DD8

surfLoads :: QDefinition
surfLoads = fromEqn' (qi ^. id) (qi ^. term) (qi ^. symbol) surfLEqn --, wi?

surfLEqn :: Expr
surfLEqn = (Int 0)

--DD9

intrsliceF :: QDefinition
intrsliceF = fromEqn' (intShrForce ^. id) (intShrForce ^. term) (intShrForce ^. symbol) intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (Int 0)

--DD10

resShearWO :: QDefinition
resShearWO = fromEqn' (ri ^. id) (ri ^. term) (ri ^. symbol) resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (Int 0)

--DD11

mobShearWO :: QDefinition
mobShearWO = fromEqn' (ti ^. id) (ti ^. term) (ti ^. symbol) mobShearWOEqn

mobShearWOEqn :: Expr
mobShearWOEqn = (Int 0)

--DD12

displcmntRxnF :: QDefinition
displcmntRxnF = fromEqn' (k_sti ^. id) (k_sti ^. term) (k_sti ^. symbol) displcmntRxnFEqn --, k_bti (correct chunk used?)

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = (Int 0)

--DD13 FIXME: id for "Net Force-Displacement Equilibrium"

netFDsplcmntEqbm :: QDefinition
netFDsplcmntEqbm = fromEqn' (ti ^. id) (ti ^. term) (ti ^. symbol) netFDsplcmntEqbmEqn

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = (Int 0)

--DD14

soilStiffness :: QDefinition
soilStiffness = fromEqn' (ti ^. id) (ti ^. term) (ti ^. symbol) --FIXME: No equation section? Instead, there are "Input" and "Output" sections
  soilStiffnessEqn

soilStiffnessEqn :: Expr
soilStiffnessEqn = (Int 0)