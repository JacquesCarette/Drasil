module Drasil.SSP.DataDefs where

import Control.Lens ((^.))
import Prelude hiding (id)

import Language.Drasil
import Drasil.SSP.Unitals
import Data.Drasil.SI_Units
import Data.Drasil.Utils

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
sliceWght = fromEqn (slcWght ^. id) (slcWght ^. term) (slcWght ^. symbol) newton slcWgtEqn

slcWgtEqn :: Expr
slcWgtEqn = (C baseWthX) * (Case [case1,case2,case3])
  where case1 = (((C slopeHght)-(C slipHght ))*(C satWeight),(C waterHght) :> (C slopeHght))
        case2 = (((C slopeHght)-(C waterHght))*(C dryWeight) + ((C waterHght)-(C slipHght))*(C satWeight),
                (C slopeHght) :> (C waterHght) :> (C slipHght))
        case3 = (((C slopeHght)-(C slipHght ))*(C dryWeight),(C waterHght) :< (C slipHght))
--FIXME: add the long equation

--DD2

baseWtrF :: QDefinition
baseWtrF = mkDataDef' baseHydroForce bsWtrFEqn 

bsWtrFEqn :: Expr
bsWtrFEqn = (C baseLngth)*(Case [case1,case2])
  where case1 = (((C waterHght)-(C slipHght))*(C waterWeight),(C waterHght) :> (C slipHght))
        case2 = (Int 0, (C waterHght) :< (C slipHght))

--DD3

surfWtrF :: QDefinition
surfWtrF = mkDataDef' surfHydroForce surfWtrFEqn

surfWtrFEqn :: Expr
surfWtrFEqn = (C surfLngth)*(Case [case1,case2])
  where case1 = (((C waterHght)-(C slopeHght))*(C waterWeight),(C waterHght) :> (C slopeHght))
        case2 = (Int 0, (C waterHght) :< (C slopeHght))

--DD4

intersliceWtrF :: QDefinition
intersliceWtrF = fromEqn' (watrForce ^. id) (watrForce ^. term) (watrForce ^. symbol) intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = (Int 0)

--DD5

angles :: QDefinition
angles = fromEqn' (baseAngle ^. id) (baseAngle ^. term) (baseAngle ^. symbol) anglesEqn --, surfAngle?

anglesEqn :: Expr
anglesEqn = (Int 0)

--DD6

lengths :: QDefinition
lengths = fromEqn' (baseLngth ^. id) (baseLngth ^. term) (baseLngth ^. symbol) lengthsEqn --, surfLngth?, bi?

lengthsEqn :: Expr
lengthsEqn = (Int 0)

--DD7

seismicLoadF :: QDefinition
seismicLoadF = fromEqn' (earthqkLoadFctr ^. id) (earthqkLoadFctr ^. term) (earthqkLoadFctr ^. symbol) ssmcLFEqn --correct chunk referenced for definition?

ssmcLFEqn :: Expr
ssmcLFEqn = (Int 0)

--DD8

surfLoads :: QDefinition
surfLoads = fromEqn' (surfLoad ^. id) (surfLoad ^. term) (surfLoad ^. symbol) surfLEqn --, slcWght?

surfLEqn :: Expr
surfLEqn = (Int 0)

--DD9

intrsliceF :: QDefinition
intrsliceF = fromEqn' (intShrForce ^. id) (intShrForce ^. term) (intShrForce ^. symbol) intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (Int 0)

--DD10

resShearWO :: QDefinition
resShearWO = fromEqn' (shearRNoIntsl ^. id) (shearRNoIntsl ^. term) (shearRNoIntsl ^. symbol) resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (Int 0)

--DD11

mobShearWO :: QDefinition
mobShearWO = fromEqn' (shearFNoIntsl ^. id) (shearFNoIntsl ^. term) (shearFNoIntsl ^. symbol) mobShearWOEqn

mobShearWOEqn :: Expr
mobShearWOEqn = (Int 0)

--DD12

displcmntRxnF :: QDefinition
displcmntRxnF = fromEqn' (shrStiffIntsl ^. id) (shrStiffIntsl ^. term) (shrStiffIntsl ^. symbol) displcmntRxnFEqn --, shrStiffBase (correct chunk used?)

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = (Int 0)

--DD13 FIXME: id for "Net Force-Displacement Equilibrium"

netFDsplcmntEqbm :: QDefinition
netFDsplcmntEqbm = fromEqn' (genForce ^. id) (genForce ^. term) (genForce ^. symbol) netFDsplcmntEqbmEqn

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = (Int 0)

--DD14

soilStiffness :: QDefinition
soilStiffness = fromEqn' (shearFNoIntsl ^. id) (shearFNoIntsl ^. term) (shearFNoIntsl ^. symbol) --FIXME: No equation section? Instead, there are "Input" and "Output" sections
  soilStiffnessEqn

soilStiffnessEqn :: Expr
soilStiffnessEqn = (Int 0)