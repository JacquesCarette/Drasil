module Drasil.SSP.DataDefs where

import Prelude hiding (id, cos, sin, tan)

import Language.Drasil
import Drasil.SSP.Unitals
import Data.Drasil.Utils
import qualified Data.Drasil.Quantities.SolidMechanics as SM

-- Needed for derivations
import Data.Drasil.Concepts.Documentation
import Data.Drasil.SentenceStructures
import Control.Lens ((^.))
import Data.Drasil.Concepts.Math (equation, angle)

------------------------
--  Data Definitions  --
------------------------

sspDataDefs :: [QDefinition]
sspDataDefs = [sliceWght, baseWtrF, surfWtrF, intersliceWtrF, angles,
  lengths, seismicLoadF, surfLoads, intrsliceF, resShearWO, mobShearWO,
  displcmntRxnF, netFDsplcmntEqbm, shearStiffness, soilStiffness]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"

--DD1

sliceWght :: QDefinition
sliceWght = mkDataDef slcWght slcWgtEqn

slcWgtEqn :: Expr
slcWgtEqn = (C baseWthX) * (Case [case1,case2,case3])
  where case1 = (((C slopeHght)-(C slipHght ))*(C satWeight),(C waterHght) :>= (C slopeHght))
        case2 = (((C slopeHght)-(C waterHght))*(C dryWeight) + ((C waterHght)-(C slipHght))*(C satWeight),
                (C slopeHght) :> (C waterHght) :> (C slipHght))
        case3 = (((C slopeHght)-(C slipHght ))*(C dryWeight),(C waterHght) :<= (C slipHght))

--DD2

baseWtrF :: QDefinition
baseWtrF = mkDataDef baseHydroForce bsWtrFEqn 

bsWtrFEqn :: Expr
bsWtrFEqn = (C baseLngth)*(Case [case1,case2])
  where case1 = (((C waterHght)-(C slipHght))*(C waterWeight),(C waterHght) :> (C slipHght))
        case2 = (Int 0, (C waterHght) :<= (C slipHght))

--DD3

surfWtrF :: QDefinition
surfWtrF = mkDataDef surfHydroForce surfWtrFEqn

surfWtrFEqn :: Expr
surfWtrFEqn = (C surfLngth)*(Case [case1,case2])
  where case1 = (((C waterHght)-(C slopeHght))*(C waterWeight),(C waterHght) :> (C slopeHght))
        case2 = (Int 0, (C waterHght) :<= (C slopeHght))

--DD4

intersliceWtrF :: QDefinition
intersliceWtrF = mkDataDef watrForce intersliceWtrFEqn

intersliceWtrFEqn :: Expr
intersliceWtrFEqn = Case [case1,case2,case3]
  where case1 = (((C slopeHght)-(C slipHght )):^(Int 2):/(Int 2) * (C satWeight) +
                 ((C waterHght)-(C slopeHght)):^(Int 2) * (C satWeight),
                (C waterHght) :>= (C slopeHght))
        case2 = (((C waterHght)-(C slipHght )):^(Int 2):/(Int 2) * (C satWeight),
                (C slopeHght) :> (C waterHght) :> (C slipHght))
        case3 = (Int 0,(C waterHght) :<= (C slipHght))

--DD5

angles :: QDefinition
angles = mkDataDef baseAngle anglesEqn --, surfAngle?

anglesEqn :: Expr
anglesEqn = ((C slipHght) - (C slipHght)) / ((C slipHght) - (C slipHght))
--FIXME: x_slip,i and x_us,i are not defined, cannot put two equations here,
--       need a way to index

--DD6

lengths :: QDefinition
lengths = mkDataDef baseWthX lengthsEqn --, baseLngth, surfLngth?

lengthsEqn :: Expr
lengthsEqn = (C slipHght) - (C slipHght)
--(C baseLngth) = (C baseWthX) * sec (C baseAngle)
--(C surfLngth) = (C baseWthX) * sec (C surfAngle)

--DD7

seismicLoadF :: QDefinition
seismicLoadF = mkDataDef earthqkLoadFctr ssmcLFEqn --correct chunk referenced for definition?

ssmcLFEqn :: Expr
ssmcLFEqn = ((C earthqkLoadFctr) * (C slcWght)) 
--FIXME: need index/ subscript changes

--DD8

surfLoads :: QDefinition
surfLoads = mkDataDef surfLoad surfLEqn --, slcWght?

surfLEqn :: Expr
surfLEqn = (C surfLoad) * (C impLoadAngle) --FIXME: Should actually just be seperated with ','

--DD9

intrsliceF :: QDefinition
intrsliceF = mkDataDef intShrForce intrsliceFEqn

intrsliceFEqn :: Expr
intrsliceFEqn = (C normToShear) * (C scalFunc) * (C intNormForce)

--DD10

resShearWO :: QDefinition
resShearWO = mkDataDef shearRNoIntsl resShearWOEqn

resShearWOEqn :: Expr
resShearWOEqn = (((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) + 
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForceDif) + (C surfHydroForce)
  :* sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle)) - (C baseHydroForce)) *
  tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle)

--DD11

mobShearWO :: QDefinition
mobShearWO = mkDataDef shearFNoIntsl mobShearWOEqn

mobShearWOEqn :: Expr 
mobShearWOEqn = ((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) + 
  (C surfLoad) * (cos (C impLoadAngle))) * (sin (C baseAngle)) - 
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForceDif) + (C surfHydroForce)
  :* sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (cos (C baseAngle))

--DD12

displcmntRxnF :: QDefinition
displcmntRxnF = mkDataDef shrStiffIntsl displcmntRxnFEqn --, shrStiffBase (correct chunk used?)

displcmntRxnFEqn :: Expr
displcmntRxnFEqn = dgnl2x2 (C shrStiffIntsl) (C nrmStiffBase) * vec2D (C dx_i) (C dy_i)

--DD13 FIXME: id for "Net Force-Displacement Equilibrium"

netFDsplcmntEqbm :: QDefinition
netFDsplcmntEqbm = mkDataDef genForce netFDsplcmntEqbmEqn

netFDsplcmntEqbmEqn :: Expr
netFDsplcmntEqbmEqn = Neg (inx surfLngth (-1)) * (inx nrmStiffIntsl (-1)) * (inx genDisplace (-1)) +
  (inx surfLngth (-1) * inx nrmStiffIntsl (-1) + inx baseLngth 0 * inx nrmStiffBase 0 +
  inx surfLngth 0 * inx nrmStiffIntsl 0) * (inx genDisplace 0) -
  (inx surfLngth 0) * (inx nrmStiffIntsl 0) * (inx genDisplace 1)

--DD14

shearStiffness :: QDefinition
shearStiffness = mkDataDef shrStiffBase shearStiffnessEqn  

shearStiffnessEqn :: Expr
shearStiffnessEqn = C intNormForce / (Int 2 * (Int 1 + C poissnsRatio)) * (Dbl 0.1 / C baseWthX) +
  (C cohesion - C normStress * tan(C fricAngle)) / (abs (C shrDispl) + C constant_a)

--DD15 this is the second part to the original DD14

soilStiffness :: QDefinition
soilStiffness = mkDataDef nrmStiffBase soilStiffnessEqn

soilStiffnessEqn :: Expr
soilStiffnessEqn = (Case [case1,case2])
  where case1 = (block, (C SM.poissnsR) :< (Int 0))
        case2 = ((Dbl 0.01) * block + (C constant_K) / ((C nrmDispl)+(C constant_A)),
                (C SM.poissnsR) :>= (Int 0))
        block = (C intNormForce)*((Int 1)-(C SM.poissnsR))/
                (((Int 1)+(C SM.poissnsR)) * ((Int 1) - (Int 2):*(C SM.poissnsR) + (C baseWthX)))

-----------------
-- Derivations --
-----------------

-- FIXEME: move derivations with the appropriate data definition

resShrDerivation :: [Contents]
resShrDerivation = [foldlSP [S "The", phrase shrResI, S "of a slice is", 
  S "defined as", getS shrResI, S "in" +:+. acroGD 3, S "The", phrase nrmFSubWat,
  S "in the", phrase equation, S "for", getS shrResI, S "of the soil is defined", 
  S "in the perpendicular force equilibrium of a slice from", acroGD 2 `sC` 
  S "using the", getTandS nrmFSubWat, S "of", acroT 4, S "shown in", eqN 1],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) + 
  (C surfHydroForce) * (cos (C surfAngle)) + --FIXME: add indexing
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C intNormForce) + (C intNormForce) -
  (C watrForce) + (C watrForce) + (C surfHydroForce) * sin (C surfAngle) + 
  (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle)) - (C baseHydroForce)),
  
  foldlSP [S "values" `ofThe'` S "interslice forces", getS intNormForce `sAnd`
  getS intShrForce, S "in the", phrase equation, S "are unknown, while the other",
  plural value, S "are found from the physical force", plural definition,
  S "of", acroDD 1, S "to" +:+. acroDD 9,
  S "Consider a force equilibrium without the affect of interslice forces" `sC`
  S "to obtain a solvable value as done for", getS nrmFNoIntsl, S "in", eqN 2],

  EqnBlock $
  (C nrmFNoIntsl) := (((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) +
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForce) + (C watrForce) +
  (C surfHydroForce) * sin (C surfAngle) +
  (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle)) - (C baseHydroForce)),
  
  foldlSP [S "Using", getS nrmFNoIntsl `sC` S "a", phrase shearRNoIntsl,
  shearRNoIntsl ^. defn, S "can be solved for in terms of all known",
  S "values as done in", eqN 3],
  
  EqnBlock $
  C shearRNoIntsl := (C nrmFNoIntsl) * tan (C fricAngle) +
  (C cohesion) * (C baseWthX) * sec (C baseAngle) := (((C slcWght) + (C surfHydroForce) * (cos (C surfAngle)) +
  (C surfLoad) * (cos (C impLoadAngle))) * (cos (C baseAngle)) +
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C watrForceDif) + (C surfHydroForce)
  * sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (sin (C baseAngle))
  - (C baseHydroForce)) *
  tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle)
  ]

mobShrDerivation :: [Contents]
mobShrDerivation = [foldlSP [S "The", phrase mobShrI, S "acting on a slice is",
  S "defined as", getS mobShrI, S "from the force equilibrium in", acroGD 2 `sC`
  S "also shown in", eqN 4],
  
  EqnBlock $
  (C nrmFSubWat) := (((C slcWght) - (C intShrForce) + (C intShrForce) +
  (C surfHydroForce) * (cos (C surfAngle)) + --FIXME: add indexing
  (C surfLoad) * (cos (C impLoadAngle))) * (sin (C baseAngle)) -
  (Neg (C earthqkLoadFctr) * (C slcWght) - (C intNormForce) + (C intNormForce)
  - (C watrForce) + (C watrForce) + (C surfHydroForce)
  * sin (C surfAngle) + (C surfLoad) * (sin (C impLoadAngle))) * (cos (C baseAngle))),
  
  foldlSP [S "The", phrase equation, S "is unsolvable, containing the unknown",
  getTandS intNormForce, S "and" +:+. getTandS intShrForce, S "Consider a force", 
  S "equilibrium", S wiif `sC` S "to obtain the", getTandS shearFNoIntsl `sC` 
  S "as done in", eqN 5], --FIXME: use wiif from shearFNoIntsl's definition but removed index
  
  EqnBlock $
  C shearFNoIntsl := ((C slcWght) :+ (C surfHydroForce) :* (cos (C surfAngle)) :+ 
  (C surfLoad) :* (cos (C impLoadAngle))) :* (sin (C baseAngle)) :- 
  (Neg (C earthqkLoadFctr) :* (C slcWght) :- (C watrForceDif) :+ (C surfHydroForce)
  :* sin (C surfAngle) :+ (C surfLoad) :* (sin (C impLoadAngle))) :* (cos (C baseAngle)),
  
  foldlSP [S "The", plural value, S "of", getS shearRNoIntsl `sAnd` getS shearFNoIntsl,
  S "are now defined completely in terms of the known force property", plural value,
  S "of", acroDD 1, S "to", acroDD 9]
  ]

stfMtrxDerivation :: [Contents]
stfMtrxDerivation = [foldlSP [S "Using the force-displacement relationship of", 
  acroGD 8, S "to define stiffness matrix", getS shrStiffIntsl `sC` S "as seen in",
  eqN 6], --FIXME: index
  
  EqnBlock $ C shrStiffIntsl := dgnl2x2 (C shrStiffIntsl) (C nrmStiffBase),
  
  foldlSP [S "For interslice surfaces the stiffness constants and displacements",
  S "refer to an unrotated coordinate system" `sC` getS genDisplace, S "of" +:+.
  acroGD 9, S "The interslice elements are left in their standard coordinate system" `sC`
  S "and therefore are described by the same", phrase equation, S "from" +:+. acroGD 8,
  S "Seen as", getS shrStiffIntsl, S "in" +:+. acroDD 12, isElMx shrStiffIntsl "shear" `sC` --FIXME: Index
  S "and", isElMx nrmStiffIntsl "normal" `sC` S "calculated as in", acroDD 14],
  
  foldlSP [S "For basal surfaces the stiffness constants and displacements refer",
  S "to a system rotated for the base angle alpha" +:+. sParen (acroDD 5),
  S "To analyze the effect of force-displacement relationships occurring on both basal",
  S "and interslice surfaces of an", phrase element, getS index, S "they must reference", 
  S "the same coordinate system. The basal stiffness matrix must be rotated", 
  S "counter clockwise to align with" +:+. (phrase angle `ofThe` S "basal surface"),
  S "The base stiffness counter clockwise rotation is applied in", eqN 7,
  S "to the new matrix", getS nrmFNoIntsl],
  
  EqnBlock $ C shrStiffIntsl := --FIXME: Index
  m2x2 (cos(C baseAngle)) (Neg $ sin(C baseAngle)) (sin(C baseAngle)) (cos(C baseAngle)) *
  C shrStiffIntsl :=
  m2x2 (C shrStiffBase * cos(C baseAngle)) (Neg $ C nrmStiffBase * sin(C baseAngle))
  (C shrStiffBase * sin(C baseAngle)) (C nrmStiffBase * cos(C baseAngle)),
  
  foldlSP [S "The Hooke's law force displacement relationship of", acroGD 8,
  S "applied to the base also references a displacement vector", getS rotatedDispl,
  S "of", acroGD 9, S "rotated for", S "base angle" `ofThe` S "slice", 
  getS baseAngle +:+. S "The basal displacement vector", getS genDisplace, 
  S "is rotated clockwise to align with the interslice displacement vector",
  getS genDisplace `sC` S "applying the", phrase definition, S "of", 
  getS rotatedDispl, S "in terms of", getS genDisplace, S "as seen in" +:+. acroGD 9,
  S "Using this with base stiffness matrix", getS shrStiffBase --FIXME: index, should be K*i"
  `sC` S "a basal force displacement relationship in the same coordinate system",
  S "as the interslice relationship can be derived as done in", eqN 8],
  
  EqnBlock $ vec2D (C genPressure) (C genPressure) := C shrStiffBase * C rotatedDispl := --FIXME: pull from other equations? index
  m2x2 (C shrStiffBase * cos(C baseAngle)) (Neg $ C nrmStiffBase * sin(C baseAngle))
  (C shrStiffBase * sin(C baseAngle)) (C nrmStiffBase * cos(C baseAngle)) *
  m2x2 (cos(C baseAngle)) (sin(C baseAngle)) (Neg $ sin(C baseAngle)) (cos(C baseAngle)) *
  vec2D (C dx_i) (C dy_i) := m2x2
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2) *
  vec2D (C dx_i) (C dy_i),
  
  foldlSP [S "The new effective base stiffness matrix", getS shrStiffBase, --FIXME: index
  S "as derived in", eqN 7, S "is defined in" +:+. eqN 9, S "This is seen as matrix",
  getS shrStiffBase, S "in" +:+. acroGD 12, isElMx shrStiffBase "shear" `sC` S "and",
  isElMx nrmStiffBase "normal" `sC` S "calculated as in" +:+. acroDD 14,
  S "The notation is simplified by", S "introduction" `ofThe` S "constants",
  getS shrStiffBase `sAnd` getS shrStiffBase `sC` S "defined in", eqN 10 `sAnd`--FIXME: index should be KbA,i and KbB,i
  eqN 11, S "respectively"],
  
  EqnBlock $ C shrStiffBase := m2x2
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  ((C shrStiffBase - C nrmStiffBase) * sin(C baseAngle) * cos(C baseAngle))
  (C shrStiffBase * cos(C baseAngle) :^ Int 2 + C nrmStiffIntsl * sin(C baseAngle) :^ Int 2)
  := m2x2 (C shrStiffBase) (C nrmStiffBase) (C nrmStiffBase) (C shrStiffBase),
  
  EqnBlock $
  (C shrStiffBase) := (C shrStiffBase) * (cos (C baseAngle)) :^ (Int 2) :+ --FIXME: the first symbol should be K_(bA,i), waiting on indexing
  (C nrmStiffBase) * (sin (C baseAngle)) :^ (Int 2),
  
  EqnBlock $
  (C shrStiffBase) := ((C shrStiffBase)-(C nrmStiffBase)) * --FIXME: the first symbol should be K_(bB,i), waiting on indexing
  (sin (C baseAngle)) * (cos (C baseAngle)),
  
  foldlSP [S "A force-displacement relationship for an element", getS index,
  S "can be written in terms of displacements occurring in the unrotated", 
  S "coordinate system", getS genDisplace `sOf` acroGD 9, S "using the matrix",
  getS shrStiffBase `sC` --FIXME: index 
  S "and", getS shrStiffBase, S "as seen in", acroDD 12]
  ]

isElMx :: (SymbolForm a) => a -> String -> Sentence
isElMx sym kword = getS sym `isThe` S kword +:+ S "element in the matrix"