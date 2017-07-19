module Drasil.SSP.IMods where

import Prelude hiding (tan, product, sin, cos)

import Language.Drasil
import Drasil.SSP.Unitals
import Drasil.SSP.Defs
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Data.Drasil.Utils

-- Needed for derivations
import Data.Drasil.Concepts.Documentation
import Data.Drasil.SentenceStructures
import Control.Lens ((^.))
import Data.Drasil.Concepts.Math (equation)
import Data.Drasil.Concepts.Physics (displacement)

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
    C index := (Int 1)),
  (((C mobShrC) * (C intNormForce) :+ (C fs) * (C shearFNoIntsl) :- (C shearRNoIntsl)):/ (C shrResC),
    (Int 1) :<= C index :<= ((C numbSlices) :- (Int 1))),
  ((Int 0), C index := (Int 0) :|| C index := C numbSlices)]
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
  (C surfHydroForce)*sin(C surfAngle) + (C surfLoad)*sin(C impLoadAngle) :=
  C dx_i * (Neg (C surfLngth) * C nrmStiffIntsl) +
  C dx_i * (Neg (C surfLngth) * C nrmStiffIntsl + C surfLngth * C nrmStiffIntsl + C baseLngth * C nrmStiffIntsl) +
  C dx_i * (Neg (C surfLngth) * C nrmStiffIntsl) +
  C dy_i * (Neg (C baseLngth) * C nrmStiffIntsl)
  :=
  Neg (C slcWght) - (C baseHydroForce)*(cos(C baseAngle)) +
  (C surfHydroForce)*cos(C surfAngle) + (C surfLoad)*cos(C impLoadAngle) :=
  C dy_i * (Neg (C surfLngth) * C nrmStiffIntsl) +
  C dy_i * (Neg (C surfLngth) * C nrmStiffIntsl + C surfLngth * C nrmStiffIntsl + C baseLngth * C nrmStiffIntsl) +
  C dy_i * (Neg (C surfLngth) * C nrmStiffIntsl) +
  C dx_i * (Neg (C baseLngth) * C nrmStiffIntsl)
  --FIXME: index fixes

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
  --FIXME: add the other long equation, see derivation equation 31

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
  
-----------------
-- Derivations --
-----------------

-- FIXEME: move derivations with the appropriate instance model

fctSftyDerivation, nrmShrDerivation, intrSlcDerivation,
  rigDisDerivation, rigFoSDerivation :: [Contents]

fctSftyDerivation = [foldlSP [S "Using", eqN 21, S "from", acroIM 3 `sC`
  S "rearranging, and", boundaryCon `sC` S "an", phrase equation, 
  S "for the", phrase fs, S "is found as", eqN 12 `sC` 
  S "also seen in", acroIM 1],
  
  EqnBlock fcSfty_rel,
  
  fUnknowns]

boundaryCon :: Sentence
boundaryCon = foldlSent_ [S "applying the boundary condition that", --FIXME: Index
  getS intNormForce `sAnd` getS intNormForce,  S "are equal to", E $ Int 0]

fUnknowns :: Contents
fUnknowns = foldlSP [S "The constants", getS mobShrC `sAnd` getS shrResC, 
  S "described in", eqN 20 `sAnd` eqN 19, S "are functions of the unknowns: the",
  getTandS normToShear, sParen (acroIM 2) `andThe` getTandS fs, sParen (acroIM 1)]

nrmShrDerivation = [foldlSP [S "Taking the last static", phrase equation,
  S "of", acroT 2, S "with the", S "moment equilibrium" `sOf` acroGD 6, S "about", 
  (S "midpoint" `ofThe` S "base") `sAnd` S "the", phrase assumption, S "of",
  acroGD 5, S "results in", eqN 13],
  
  EqnBlock $ Int 0 :=
  Neg (C intNormForce) :* (C sliceHght :- C baseWthX :/ Int 2 :* 
  tan (C baseAngle)) :+ C intNormForce :* (C sliceHght :- 
  C baseWthX :/ Int 2 :* tan (C baseAngle)) :- C watrForce :*
  (C sliceHght :- C baseWthX :/ Int 2 :* tan (C baseAngle)) :+ 
  C watrForce :* (C sliceHght :- C baseWthX :/ Int 2 :* 
  tan (C baseAngle)) :- C normToShear * (C baseWthX :/ Int 2) :* 
  (C intNormForce * C scalFunc + C intNormForce * C scalFunc) :+
  C earthqkLoadFctr :* C slcWght :* C midpntHght :/ Int 2 :-
  C surfHydroForce :* sin (C surfAngle) :* C midpntHght :-
  C surfLoad :* sin (C impLoadAngle) :* C midpntHght,
  
  foldlSP [S "The", phrase equation, S "in terms of", getS normToShear, S "leads to", eqN 14],
  
  EqnBlock $
  C normToShear := 
  (Neg (C intNormForce) :* (C sliceHght :- C baseWthX :/ Int 2 :* 
  tan (C baseAngle)) :+ C intNormForce :* (C sliceHght :- 
  C baseWthX :/ Int 2 :* tan (C baseAngle)) :- C watrForce :*
  (C sliceHght :- C baseWthX :/ Int 2 :* tan (C baseAngle)) :+ 
  C watrForce :* (C sliceHght :- C baseWthX :/ Int 2 :* 
  tan (C baseAngle)) :+ C earthqkLoadFctr :* 
  C slcWght :* C midpntHght :/ Int 2 :- C surfHydroForce :*
  sin (C surfAngle) :* C midpntHght :- C surfLoad :* 
  sin (C impLoadAngle) :* C midpntHght)
  / ((C baseWthX :/ Int 2) *
  (C intNormForce * C scalFunc + C intNormForce * C scalFunc)), 
  
  foldlSP [S "Taking a summation of each slice, and", boundaryCon `sC`
  S "a general", phrase equation, S "for the constant", getS normToShear,
  S "is developed in", eqN 15 `sC` S "also found in", acroIM 2], --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  
  EqnBlock $
  C normToShear := summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseWthX * (C intNormForce + C intNormForce + C watrForce + C watrForce) * tan(C baseAngle) +
  C midpntHght * (C earthqkLoadFctr * C slcWght - Int 2 * C surfHydroForce * sin(C surfAngle) -
  Int 2 * C surfLoad * sin(C impLoadAngle))) / 
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseWthX * (C intNormForce * C scalFunc + C intNormForce * C scalFunc)),
  
  foldlSP [eqN 15, S "for", getS normToShear `sC` S "is a function of the unknown",
  getTandS intNormForce, acroIM 3]
  ]

intrSlcDerivation = [foldlSP [S "Taking the", S "normal force equilibrium" `sOf` acroGD 1,
  S "with the", S "effective stress", phrase definition, S "from", acroT 4, --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  S "that", E (C totNrmForce := C nrmFSubWat - C baseHydroForce) `sC`
  S "and the assumption of", acroGD 5, S "the equilibrium", phrase equation, 
  S "can be rewritten as", eqN 16],
  
  EqnBlock $
  C nrmFSubWat := ((C slcWght :- C normToShear :* C scalFunc :* C intNormForce :+ 
  C normToShear :* C scalFunc :* C intNormForce :+ 
  C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
  cos (C impLoadAngle)) :* cos (C baseAngle)
  :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
  C intNormForce :+ C intNormForce :- C watrForce :+ 
  C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
  C surfLoad :* sin (C impLoadAngle)) :* sin (C baseAngle)) - (C baseHydroForce),
  
  foldlSP [S "Taking the", S "base shear force equilibrium" `sOf` acroGD 2, S "with the", phrase definition,
  S "of", phrase mobShrI, S "from", acroGD 4 `sAnd` S "the assumption of", acroGD 5 `sC`
  S "the equilibrium", phrase equation, S "can be rewritten as", eqN 17], --NOTE: "Taking this with that and the assumption of _ to get equation #" pattern
  
  EqnBlock $
  ((C totNrmForce) * tan (C fricAngle) + (C cohesion) * (C baseWthX) * sec (C baseAngle)) / (C fs) := --FIXME: pull the left side of this from GD4
  (C slcWght :- C normToShear :* C scalFunc :* C intNormForce :+ 
  C normToShear :* C scalFunc :* C intNormForce :+ 
  C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
  cos (C impLoadAngle)) :* sin (C baseAngle)
  :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
  C intNormForce :+ C intNormForce :- C watrForce :+ 
  C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
  C surfLoad :* sin (C impLoadAngle)) :* cos (C baseAngle),
  
  foldlSP [S "Substituting the", phrase equation, S "for", getS nrmFSubWat,
  S "from", eqN 16, S "into", eqN 17, S "and rearranging results in", eqN 18],

  EqnBlock $
  (C intNormForce) * (((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs)) := 
  (C intNormForce) * (((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs)) +
  (C fs) * (C shearFNoIntsl) - (C shearRNoIntsl),
  
  foldlSP [S "Where", getS shearRNoIntsl `sAnd` getS shearFNoIntsl, S "are the",
  S "resistive and mobile shear of the slice" `sC` S wiif, getS intNormForce
  `sAnd` getS intShrForce `sC` S "as defined in", acroDD 10 `sAnd` acroDD 11,
  S "Making use of the constants, and with full", plural equation, 
  S "found below in", eqN 19 `sAnd` eqN 20, S "respectively, then", eqN 18, 
  S "can be simplified to", eqN 21 `sC` S "also seen in", acroIM 3],
  
  EqnBlock $
  (C shrResC) := ((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs),
  --FIXME: index everything here and add "Where i is the local slice of mass for 1 :<= i :<= n-1"
  EqnBlock $
  (C mobShrC) := ((C normToShear)*(C scalFunc) * cos (C baseAngle) - sin (C baseAngle)) * tan (C fricAngle) -
  ((C normToShear)*(C scalFunc) * sin (C baseAngle) - cos (C baseAngle)) * (C fs),
  
  EqnBlock $
  (C intNormForce) := ((C mobShrC)*(C intNormForce) + (C fs)*(C shearFNoIntsl)
  - (C shearRNoIntsl)) / (C shrResC),
  
  fUnknowns]

rigDisDerivation = [foldlSP [S "Using the net force-displacement equilibrium",
  phrase equation, S "of a slice from", acroDD 13, S "with", plural definition
  `ofThe` S "stiffness matrices", S "from", acroDD 12, S "and the force", 
  plural definition, S "from", acroGD 7 , S "a broken down force displacement", 
  S "equilibrium", phrase equation +:+. S "can be derived",
  eqN 22, S "gives the broken down", phrase equation, S "in the x direction"
  `sC` S "and", eqN 23, S "gives the broken down", phrase equation,
  S "in the y direction"],

  EqnBlock fDisEq_rel, --FIXME: Original equations need indexing
  
  foldlSP [S "Using the known input assumption of", acroA 2 `sC` S "the force",
  S "variable", plural definition, S "of", acroDD 1, S "to", acroDD 8, S "on",
  S "left side" `ofThe` plural equation, S "can be solved for. The only unknown", 
  S "in the variables to solve for the stiffness values from", acroDD 14 +:+. 
  S "is the displacements", S "Therefore taking the", phrase equation, 
  S "from each slice a set of", E $ (Int 2) * (C numbSlices), plural equation
  `sC` S "with", E $ (2) * (C numbSlices), S "unknown displacements in the", 
  S "x and y directions of each slice can be derived. Solutions for the displacements",
  S "of each slice can then be found. The use of displacement in", phrase definition `ofThe`
  S "stiffness values makes the", phrase equation, S "implicit, which means an iterative solution",
  S "method, with an initial guess for the displacements in the stiffness", plural value,
  S "is required"]
  ]

rigFoSDerivation = [foldlSP [S "RFEM analysis can also be used to calculate the",
  phrase fs, S "for the slope. For a slice element", getS index, S "the displacements",
  getS dx_i `sAnd` getS dy_i `sC` S "are solved from the system of", plural equation, 
  S "in" +:+. acroIM 4, S "The", phrase definition, S "of", getS rotatedDispl, S "as", 
  S "rotation" `ofThe` S "displacement vector", getS genDisplace, S "is seen in" +:+.
  acroGD 9, S "This is", --FIXME: index i 
  S "used to find", plural displacement `ofThe` S "slice parallel to", 
  S "base" `ofThe` S "slice", getS shrDispl `sIn` eqN 24, S "and normal to", 
  S "base" `ofThe` S "slice", getS nrmDispl, S "in", eqN 25],
  
  EqnBlock $
  C shrDispl := cos(C baseAngle) * C dx_i + sin(C baseAngle) * C dy_i,
  EqnBlock $
  C nrmDispl := Neg (sin(C baseAngle)) * C dx_i + sin(C baseAngle) * C dy_i,
  
  foldlSP [S "With the", phrase definition, S "of normal stiffness from", acroDD 14, --FIXME: grab nrmStiffBase's term name?
  S "to find", S "normal stiffness" `ofThe` S "base", getS nrmStiffBase,
  S "and the now known base displacement perpendicular to the surface",
  getS nrmDispl, S "from", eqN 25, S "the normal base stress",
  S "can be calculated from the force-displacement relationship of" +:+. acroT 5,
  S "Stress", getS normStress `sIs` S "used in place of", getTandS genForce, --FIXME: use getTandS
  S "as the stiffness hasn't been normalized for" +:+. (S "length" `ofThe` S "base"), 
  S "Results" `sIn` eqN 26], --FIXME: grammar

  EqnBlock $
  C normStress := C nrmStiffBase * C nrmDispl, --FIXME: index
  
  foldlSP [S "The resistive shear to calculate the", getTandS fs,
  S "is found from the Mohr Coulomb resistive strength of soil in", acroT 3,
  S "Using the", getTandS normStress, S "from", eqN 26, S "as the stress" `sC`
  (S "resistive shear" `ofThe` S "slice"), S "can be calculated from", eqN 27],
  
  EqnBlock $
  C mobStress := C cohesion - C normStress * tan(C fricAngle), --FIXME: index and prime
  
  foldlSP [S "Previously", phrase value `ofThe` getTandS shrStiffBase,
  S "as seen in", eqN 28, S "was unsolvable because the", getTandS normStress,
  S "was unknown. With the", phrase definition, S "of", getS normStress, S "from", eqN 26,
  S "and the", phrase definition, S "of displacement shear to the base", getS shrDispl,
  S "from", eqN 25 `sC` S "the value of", getS shrStiffBase, S "becomes solvable"],
  
  EqnBlock $
  C shrStiffBase := C intNormForce / (Int 2 * (Int 1 + C poissnsRatio)) * (Dbl 0.1 / C baseWthX) +
  (C cohesion - C normStress * tan(C fricAngle)) / (abs (C shrDispl) + C constant_a),
  
  foldlSP [S "With", getTandS shrStiffBase, S "calculated in", eqN 28,
  S "and shear displacement", getS shrDispl, S "calculated in", eqN 24, --FIXME: grab term too once we have a displacement modifier
  S "values now known the", phrase shrStress, shrStress ^. defn, getS shrStress,
  S "can be calculated using", acroT 5 `sC` S "as done in" +:+. eqN 29,
  S "Again, stress", getS shrStress, S "is used in place of force", getS genForce, --FIXME: grab term
  S "as the stiffness has not been normalized for", S "length" `ofThe` S "base"],
  
  EqnBlock $
  C shrStress := C shrStiffBase * C shrDispl,
  
  foldlSP [S "The", phrase shrStress, shrStress ^. defn, getS shrStress, --FIXME: ISSUE #348
  S "acts as the mobile shear acting on the base. Using the", phrase definition,
  titleize fs, phrase equation, S "from", acroT 1 `sC` S "with the", 
  plural definition, S "of resistive shear strength of a slice", getS mobStress,
  S "from", phrase equation, S "(27) and shear stress on a slice", getS shrStress, S "from",
  eqN 29, S "the", getTandS fsloc, S "can be found from as seen in", eqN 30 `sAnd` acroIM 5],
  
  EqnBlock $
  C fsloc := C mobStress / C shrStress :=
  (C cohesion - C nrmStiffBase * C nrmDispl * tan(C fricAngle)) /
  (C shrStiffBase * C shrDispl), --FIXME: pull parts of this equation from other equations such as IM5
  
  foldlSP [S "The global", titleize fs, S "is then", S "ratio" `ofThe` S "summation",
  S "of the resistive and mobile shears for each slice, with a weighting for" +:+.
  (S "length" `ofThe` S "slice's base"), S "Shown in", eqN 31 `sAnd` acroIM 5],
  
  EqnBlock $ --FIXME: pull from other equations in derivation
  (C fs) := summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * C mobStress) /
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * C shrStress) :=
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * (C cohesion - C nrmStiffBase * C nrmDispl * tan(C fricAngle))) /
  summation (Just (lI, Low $ Int 1, High $ C numbSlices))
  (C baseLngth * (C shrStiffBase * C shrDispl)) --FIXME: Grouping with brackets
  ]
