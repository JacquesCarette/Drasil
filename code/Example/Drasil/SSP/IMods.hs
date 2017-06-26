module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil
import Drasil.SSP.Unitals
import Drasil.SSP.Defs
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Data.Drasil.Utils

-----------------------
--  Instance Models  --
-----------------------

sspIMods :: [RelationConcept]
sspIMods = [fctSfty, nrmShrFor, intsliceFs, forDisEqlb, rfemFoS, crtSlpId]

--
fctSfty :: RelationConcept
fctSfty = makeRC "fctSfty" factorOfSafety fcSfty_desc fcSfty_rel

--FIXME: first shearRNoIntsl should have local index v, not i, last occurence should have index n
--       similar case with shearFNoIntsl
fcSfty_rel :: Relation
fcSfty_rel = (C fs) := (sumOp shearRNoIntsl :+ (C shearRNoIntsl)) :/ 
                       (sumOp shearFNoIntsl :+ (C shearFNoIntsl))
  where prodOp    = product   (Just (lC, Low $ V "i", High $ (V "n") :- (Int 1)))
                              ((C mobShrC) :/ (C shrResC))
        sumOp sym = summation (Just (lV, Low $ Int 1, High $ (V "n") :- (Int 1)))
                              ((C sym) :* prodOp)

fcSfty_desc :: Sentence
fcSfty_desc = foldlSent [S "Equation for the", S "Factor of Safety" `isThe` S "ratio",
  S "between resistive and mobile shear of the slip surface. The sum of values",
  S "from each slice is taken to find the total resistive and mobile shear for",
  S "the slip surface. The constants", getS shrResC, S "and", getS mobShrC, 
  S "convert the resistive and mobile shear without the inluence of", --FIXME: have these constents defined somewhere else
  S "interslice forces, to a calculation considering the interslice forces"]

--
nrmShrFor :: RelationConcept
nrmShrFor = makeRC "nrmShrFor" (nounPhraseSP "normal/shear force ratio") nrmShrF_desc nrmShrF_rel

nrmShrF_rel :: Relation
nrmShrF_rel = (C fs) := Case [case1,case2,case3]
  where case1 = ((C baseWthX)*((C intNormForce)+(C watrForce)) * tan (C baseAngle) , (V "i") := Int 1) --FIXME: use index i
        case2 = ((C baseWthX)*((C intNormForce)+(C intNormForce)+(C watrForce)+(C watrForce)) * tan (C baseAngle),
                Int 2 :<= (V "i") :<= ((C numbSlices) - (Int 1)))
        case3 = ((C baseWthX)*((C intNormForce)+(C watrForce)) * tan (C baseAngle) , (V "i") := (C numbSlices))

--FIXME: add the long equation

nrmShrF_desc :: Sentence
nrmShrF_desc = foldlSent [getS normToShear `isThe` S "magnitude ratio between",
  S "shear and normal forces at the interslice interfaces as the assumption of", 
  S "the Morgenstern Price method in GD5. The inclination function f determines",
  S "the relative magnitude ratio between the different interslices, while",
  getS normToShear, S "determines the" +:+. S "magnitude", getS normToShear,
  S "uses the sum of interslice normal and shear forces taken from each interslice"]
  --FIXME: "i" needs to be pulled out and used as a symbol

--
intsliceFs :: RelationConcept
intsliceFs = makeRC "intsliceFs" (nounPhraseSP "interslice forces") sliceFs_desc sliceFs_rel

sliceFs_rel :: Relation
sliceFs_rel = (C intNormForce) := Case [
  (((C fs) * (C shearFNoIntsl) :- (C shearRNoIntsl)) :/ (C shrResC),
    (Int 1) := (Int 1)),
  (((C mobShrC) * (C intNormForce) :+ (C fs) * (C shearFNoIntsl) :- (C shearRNoIntsl)):/ (C shrResC),
    (Int 1) :<= (Int 1) :<= ((C numbSlices) :- (Int 1))),
  ((Int 0), (Int 0) := (Int 0) * (V "and") * (Int 0) := (C numbSlices))]
  -- FIXME: Use index i as part of condition

sliceFs_desc :: Sentence
sliceFs_desc = foldlSent [S "The value of the interslice normal force",
  getS intNormForce, S "at interface i. The net force" `isThe` S "weight",
  S "of the slices adjacent to interface i exert horizontally on each other"]
  --FIXME: "i" needs to be pulled out and used as a symbol

--
forDisEqlb :: RelationConcept
forDisEqlb = makeRC "forDisEqlb" (nounPhraseSP "force displacement equilibrium") fDisEq_desc fDisEq_rel

fDisEq_rel :: Relation
fDisEq_rel = Neg (C watrForceDif) - (C earthqkLoadFctr)*(C slcWght) -
  (C baseHydroForce)*(sin(C baseAngle)) +
  (C surfHydroForce)*sin(C surfAngle) + (C surfLoad)*sin(C impLoadAngle) := (Int 1) 
  --FIXME: add the other long equation (i.e. Y Equilibrium)
  --FIXME: add the second part of X Equilibrium equation

fDisEq_desc :: Sentence
fDisEq_desc = foldlSent [S "There is one set of force displacement equilibrium",
  S "equations in the x and y directions for each element. System of equations",
  S "solved for displacements (", (getS dx_i), S "and",
  (getS dy_i), S ")", (getS watrForceDif), S "=", (getS watrForce)
  `isThe` S "net hydrostatic force across a slice.", (getS earthqkLoadFctr)
  `isThe` S "earthquake load factor.", (getS slcWght)
  `isThe` S "weight of the slice.", (getS baseHydroForce) 
  `isThe` S "pore water pressure acting on the slice base.", (getS surfHydroForce)
  `isThe` S "pore water pressure acting on the slice surface.", (getS baseAngle)
  `isThe` S "angle of the base with the horizontal.", (getS surfAngle)
  `isThe` S "angle of the surface with the horizontal.", (getS dx_i)
  `isThe` S "x displacement of slice i.", (getS dy_i)
  `isThe` S "y displacement of slice i.", (getS surfLngth) 
  `isThe` S "length of the interslice surface i.", (getS baseLngth)
  `isThe` S "length of the base surface i.", (getS shrStiffIntsl)
  `isThe` S "interslice shear stiffness at surface i.", S " Kst,i-1"
  `isThe` S "interslice normal stiffness at surface i. KbA,i, and KbB,i",
  S "are the base stiffness values for slice i"]

--
rfemFoS :: RelationConcept
rfemFoS = makeRC "rfemFoS" (nounPhraseSP "RFEM factor of safety") rfemFoS_desc rfemFoS_rel

rfemFoS_rel :: Relation
rfemFoS_rel = (C fsloc) := ((C cohesion):-(C nrmStiffBase)*(C nrmDispl)*(tan (C fricAngle))):/
  ((C shrStiffBase)*(C shrDispl)) 
  --FIXME: add the other long equation

rfemFoS_desc :: Sentence
rfemFoS_desc = foldlSent [(getS fsloc) `isThe` S "factor of safety for slice i.",
  (getS fs) `isThe` S "factor of safety for the entire slip surface.",
  (getS cohesion) `isThe` S "cohesion of slice i's base.",
  (getS fricAngle) `isThe` (phrase fricAngle), S "of slice i's base.",
  (getS nrmDispl) `isThe` S "normal displacement of slice i.",
  (getS shrDispl) `isThe` S "shear displacement of slice i.",
  (getS shrStiffBase) `isThe` S "length of the base of slice i.",
  (getS nrmStiffBase) `isThe` S "base normal stiffness at surface i.",
  (getS numbSlices) `isThe` S "number of slices in the slip surface"]

--
crtSlpId :: RelationConcept
crtSlpId = makeRC "crtSlpId" (nounPhraseSP "critical slip identification") crtSlpId_desc crtSlpId_rel

crtSlpId_rel :: Relation
crtSlpId_rel = (C fs) := (FCall (C minFunction) [C critCoords, V "Input"]) --FIXME: add subscript to fs

crtSlpId_desc :: Sentence
crtSlpId_desc = foldlSent [S "Given the necessary slope inputs, a minimization",
  S "algorithm or function", getS minFunction, S "will identify the", phrase crtSlpSrf,
  S "of the slope, with the critical slip coordinates", getS critCoords, 
  S "and the minimum factor of safety FSmin that results"]