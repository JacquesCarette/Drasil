module Drasil.SSP.DataDefs where

import Control.Lens ((^.))
import Prelude hiding (id, cos, sin, tan)

import Language.Drasil
import Drasil.SSP.Unitals
import Data.Drasil.SI_Units
import Data.Drasil.Utils
import qualified Data.Drasil.Quantities.SolidMechanics as SM

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
intersliceWtrFEqn = Case [case1,case2,case3]
  where case1 = (((C slopeHght)-(C slipHght )):^(Int 2):/(Int 2) :* (C satWeight) +
                 ((C waterHght)-(C slopeHght)):^(Int 2) :* (C satWeight),
                (C waterHght) :> (C slopeHght))
        case2 = (((C waterHght)-(C slipHght )):^(Int 2):/(Int 2) :* (C satWeight),
                (C slopeHght) :> (C waterHght) :> (C slipHght))
        case3 = (Int 0,(C waterHght) :< (C slipHght))

--DD5

angles :: QDefinition
angles = fromEqn' (baseAngle ^. id) (baseAngle ^. term) (baseAngle ^. symbol) anglesEqn --, surfAngle?

anglesEqn :: Expr
anglesEqn = ((C slipHght) :- (C slipHght)) :/ ((C slipHght) :- (C slipHght))
--FIXME: x_slip,i and x_us,i are not defined, cannot put two equations here,
--       need a way to index

--DD6

lengths :: QDefinition
lengths = fromEqn' (baseWthX ^. id) (baseWthX ^. term) (baseWthX ^. symbol) lengthsEqn --, baseLngth, surfLngth?

lengthsEqn :: Expr
lengthsEqn = (C slipHght) :- (C slipHght)
--(C baseLngth) := (C baseWthX) :* sec (C baseAngle)
--(C surfLngth) := (C baseWthX) :* sec (C surfAngle)

--DD7

seismicLoadF :: QDefinition
seismicLoadF = fromEqn' (earthqkLoadFctr ^. id) (earthqkLoadFctr ^. term) (earthqkLoadFctr ^. symbol) ssmcLFEqn --correct chunk referenced for definition?

ssmcLFEqn :: Expr
ssmcLFEqn = ((C earthqkLoadFctr) :* (C slcWght)) 
--FIXME: need index/ subscript changes

--DD8

surfLoads :: QDefinition
surfLoads = fromEqn' (surfLoad ^. id) (surfLoad ^. term) (surfLoad ^. symbol) surfLEqn --, slcWght?

surfLEqn :: Expr
surfLEqn = (C surfLoad) :* (C impLoadAngle) --FIXME: Should actually just be seperated with ','

--DD9

intrsliceF :: QDefinition
intrsliceF = fromEqn' (intShrForce ^. id) (intShrForce ^. term) (intShrForce ^. symbol) intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (C normToShear) :* (C scalFunc) :* (C intNormForce)

--DD10

resShearWO :: QDefinition
resShearWO = fromEqn' (shearRNoIntsl ^. id) (shearRNoIntsl ^. term) (shearRNoIntsl ^. symbol) resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (cos (C baseAngle)) :+
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (sin (C baseAngle)) :- (C baseHydroForce)) :*
  tan (C fricAngle) :+ (C cohesion) :* (C baseWthX) :* sec (C baseAngle)

--DD11

mobShearWO :: QDefinition
mobShearWO = fromEqn' (shearFNoIntsl ^. id) (shearFNoIntsl ^. term) (shearFNoIntsl ^. symbol) mobShearWOEqn

mobShearWOEqn :: Expr 
mobShearWOEqn = ((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (sin (C baseAngle)) :- 
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (cos (C baseAngle))

--DD12

displcmntRxnF :: QDefinition
displcmntRxnF = fromEqn' (shrStiffIntsl ^. id) (shrStiffIntsl ^. term) (shrStiffIntsl ^. symbol) displcmntRxnFEqn --, shrStiffBase (correct chunk used?)

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = (Int 0)

--DD13 FIXME: id for "Net Force-Displacement Equilibrium"

netFDsplcmntEqbm :: QDefinition
netFDsplcmntEqbm = fromEqn' (genForce ^. id) (genForce ^. term) (genForce ^. symbol) netFDsplcmntEqbmEqn

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = Neg (C surfLngth) * (C nrmStiffIntsl) * (C genDisplace) +
  ((C surfLngth) * (C nrmStiffIntsl) + (C baseLngth) * (C nrmStiffBase) + (C surfLngth) * (C nrmStiffIntsl)) * (C genDisplace) -
  (C surfLngth) * (C nrmStiffIntsl) * (C genDisplace) --FIXME: needs indexing

--DD14

soilStiffness :: QDefinition
soilStiffness = fromEqn' (nrmStiffRes ^. id) (nrmStiffRes ^. term) (nrmStiffRes ^. symbol) --FIXME: No equation section? Instead, there are "Input" and "Output" sections
  soilStiffnessEqn

soilStiffnessEqn :: Expr
soilStiffnessEqn = (Case [case1,case2])
  where case1 = (block, (C SM.poissnsR) :< (Int 0))
        case2 = ((Dbl 0.01) * block + (V "k") / ((C nrmDispl)+(V "A")),
                (C SM.poissnsR) :>= (Int 0))
        block = (C intNormForce)*((Int 1)-(C SM.poissnsR))/
                (((Int 1)+(C SM.poissnsR)) :* ((Int 1) :- (Int 2):*(C SM.poissnsR) :+ (C baseWthX)))