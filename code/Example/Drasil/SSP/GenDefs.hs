module Drasil.SSP.GenDefs where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Drasil.SSP.Unitals
import Data.Drasil.Concepts.Documentation
import Drasil.SSP.Defs
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Quantities.Physics
import Data.Drasil.SentenceStructures
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Quantities.SolidMechanics
import Data.Drasil.Concepts.Math
import Data.Drasil.Utils
import qualified Drasil.SRS as SRS

eqlExpr :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExpr f1_ f2_ _e_ = ((inxi slcWght) `_e_`
  (inxi surfHydroForce) * (cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) +
  (Neg (C earthqkLoadFctr) * (inxi slcWght) - (inxi intNormForce) + (inxiM1 intNormForce) -
  (inxi watrForce) + (inxiM1 watrForce) + (inxi surfHydroForce) * sin (inxi surfAngle) + 
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle))

---------------------------
--  General Definitions  --
---------------------------

sspGenDefs :: [RelationConcept]
sspGenDefs = [normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, netForcex, netForcey, hookesLaw2d, displVect]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium") nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = inxi totNrmForce := eqlExpr cos sin (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

nmFEq_desc :: Sentence
nmFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", acroT 2, S "in the direction",
  phrase perp, S "to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for", (phrase normForce `ofThe`
  phrase surface) +:+. getS totNrmForce, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRef (SRS.physSyst SRS.missingP []), S "Index i",
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRef (SRS.physSyst SRS.missingP []), at_start force, phrase variable,
  plural definition, S "can be found in", acroDD 1, S "to",
  acroDD 9]

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium") bShFEq_desc bShFEq_rel

bShFEq_rel :: Relation
bShFEq_rel = inxi mobShrI := eqlExpr sin cos (\x y -> x - inxiM1 intShrForce + inxi intShrForce + y)

bShFEq_desc :: Sentence
bShFEq_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the", phrase force,
  S "equilibrium to satisfy", acroT 2, S "in the direction",
  S "parallel to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice), S "Rearranged to solve for the", phrase shearForce,
  S "on the base" +:+. getS mobShrI, at_start force, S "equilibrium is",
  S "derived from the free body diagram of",
  makeRef (SRS.physSyst SRS.missingP []), S "Index i",
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following convention in" +:+.
  makeRef (SRS.physSyst SRS.missingP []), at_start force, phrase variable,
  plural definition, S "can be found in", acroDD 1, S "to",
  acroDD 9]

--
shrResEqn :: Expr
shrResEqn = inxi nrmFSubWat * tan (C fricAngle) + C cohesion * inxi baseWthX * sec (inxi baseAngle)

resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force") resShr_desc resShr_rel

resShr_rel :: Relation
resShr_rel = inxi shrResI := shrResEqn

resShr_desc :: Sentence
resShr_desc = foldlSent [S "The Mohr-Coulomb resistive shear strength of a",
  phrase slice, getS shrStress, S "from", acroT 3, S "is multiplied by the area",
  E $ C baseWthX * sec(C baseAngle) * 1, S "to obtain the" +:+. getTandS shrResI,
  S "Note the extra", E 1 , S "is to represent a unit of width which is",
  S "multiplied by the", getTandS baseLngth, S "of the plane where the",
  phrase normal, S "occurs, where", (E $ C baseLngth := C baseWthX * sec(C baseAngle))
  `sAnd` getS baseWthX, S "is the x width of the base. This accounts for the",
  phrase nrmFSubWat, E $ C nrmFSubWat := C totNrmForce - C baseHydroForce, S "of a soil from", -- FIXME: add prime to nrmStrss
  acroT 4, S "where the", phrase nrmStrss, S "is multiplied by the same area to obtain the",
  phrase nrmFSubWat, E $ C nrmStrss * C baseWthX * sec(C baseAngle) * 1 := C nrmFSubWat]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr" (nounPhraseSP "mobile shear force") mobShr_desc mobShr_rel

mobShr_rel :: Relation
mobShr_rel = inxi mobShrI := inxi shrResI / C fs := shrResEqn / C fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent [
  S "From", phrase definition `ofThe` ((phrase factor) `sOf` (phrase safety)), S "in", acroT 1 `sC` --FIXME: factor of saftey hacked in to avoid cyclical imports
  S "and the new", phrase definition, S "of", getS shrResI `sC` S "a new",
  S "relation for", (S "net mobile" +:+ phrase shearForce `ofThe` phrase slice),
  getS shearFNoIntsl, S "is found as the resistive shear" , getS shrResI,
  sParen (acroGD 3), S "divided by the factor of safety", getS fs]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR" (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel

nmShrR_rel :: Relation
nmShrR_rel = C intShrForce := C normToShear :* C scalFunc :* C intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent [S "The", phrase assumption, S "for the Morgenstern Price",
  phrase method_, sParen (acroA 5), S "that the", phrase intrslce,
  phrase shearForce, getS xi, S "is proportional to the", phrase intrslce, 
  phrase normForce, getS intNormForce, S "by a proportionality constant",
  getS normToShear, S "and a predetermined scaling function", --FIXME: indexing on normToShear
  getS scalFunc `sC` S "that changes", (S "proportionality as a function" `ofThe`
  S "x-ordinate position of the") +:+. phrase intrslce,
  getS scalFunc, S "is typically either a half-sine along the slip", phrase surface `sC`
  S "or a constant"]

--
momExpr :: (Expr -> Expr -> Expr) -> Expr
momExpr _e_ = (Neg (inxi intNormForce) :* (inxi sliceHght :- inxi baseWthX :/ 2 :* 
  tan (inxi baseAngle)) :+ inxiM1 intNormForce :* (inxiM1 sliceHght :- 
  inxi baseWthX :/ 2 :* tan (inxi baseAngle)) :- inxi watrForce :*
  (inxi sliceHght :- inxi baseWthX :/ 2 :* tan (inxi baseAngle)) :+ 
  inxiM1 watrForce :* (inxiM1 sliceHght :- inxi baseWthX :/ 2 :* 
  tan (inxi baseAngle))) `_e_`
  (C earthqkLoadFctr :* inxi slcWght :* inxi midpntHght :/ 2 :-
  inxi surfHydroForce :* sin (inxi surfAngle) :* inxi midpntHght :-
  inxi surfLoad :* sin (inxi impLoadAngle) :* inxi midpntHght)

momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium") momEql_desc momEql_rel

momEql_rel :: Relation
momEql_rel = (Int 0) := momExpr
  (\ x y -> x :- (inxi baseWthX :/ 2 :* (inxi intShrForce :+ inxiM1 intShrForce)) :+ y)

momEql_desc :: Sentence
momEql_desc = foldlSent [S "For a", phrase slice, S "of", phrase mass,
  S "in the", phrase slope, S "the moment equilibrium to satisfy", acroT 2,
  S "in the direction", phrase perp,
  S "to" +:+. (S "base" +:+ phrase surface `ofThe` phrase slice),
  S "Moment equilibrium is derived from the free body diagram of" +:+.
  makeRef (SRS.physSyst SRS.missingP []), S "Index i refers to",
  plural value `ofThe` plural property, S "for", phrase slice :+: S "/" :+:
  plural intrslce, S "following convention in" +:+.
  makeRef (SRS.physSyst SRS.missingP []), at_start variable, plural definition,
  S "can be found in", acroDD 1, S "to", acroDD 9]

--
netForcex :: RelationConcept
netForcex = makeRC "netForce" (nounPhraseSP "net x-component force") EmptyS fNetx_rel

fNetx_rel :: Relation
fNetx_rel = inxi fx := (Neg $ inxi watrForceDif) - (C earthqkLoadFctr)*(inxi slcWght)
  - (inxi baseHydroForce) * sin (inxi baseAngle) + (inxi surfHydroForce) * sin (inxi surfAngle)
  + (inxi surfLoad) * sin (inxi impLoadAngle)

netForcey :: RelationConcept
netForcey = makeRC "netForce" (nounPhraseSP "net y-component force") fNet_desc fNety_rel

fNety_rel :: Relation
fNety_rel = inxi fy := (Neg $ inxi slcWght) + (inxi baseHydroForce) * cos (inxi baseAngle)
  - (inxi surfHydroForce) * cos (inxi surfAngle) - (inxi surfLoad) * cos (inxi impLoadAngle)


fNet_desc :: Sentence
fNet_desc = foldlSent [S "The net sum of", plural force, S "acting on a",
  phrase slice, S "for the RFEM" +:+. phrase model, S "The", plural force,
  S "that create an applied load on the" +:+. phrase slice,
  -- FIXME: Sentence does not sound like a sentence
  S "Fx,i refers to the load in the direction", phrase perp, S "to the", --FIXME: Index force
  S "direction of the", phrase force, S "of gravity for", phrase slice,
  S "i, while Fy,i refers to the load in the direction parallel to the",
  phrase force, S "of gravity for", phrase slice,
  S "i.", at_start' force, S "are found in the free body diagram of" +:+.
  makeRef (SRS.physSyst SRS.missingP []), S "In this", phrase model,
  S "the", plural element, S "are not exerting", plural force,
  S "on each other" `sC` S "so the", phrase intrslce, plural force,
  getS intNormForce, S "and", getS intShrForce, S "are not a part of the" +:+. phrase model, S "Index i", 
  S "refers to", (plural value `ofThe` plural property), S "for",
  phrase slice :+: S "/" :+: plural intrslce, S "following", 
  S "convention in" +:+. makeRef (SRS.physSyst SRS.missingP []), 
  at_start force, phrase variable, plural definition, S "can be found in",
  acroDD 1, S "to", acroDD 8]
  --FIXME:Finish pulling out symbols

--
hookesLaw2d :: RelationConcept
hookesLaw2d = makeRC "hookesLaw2d" (nounPhraseSP "Hooke's law 2D") hooke2d_desc hooke2d_rel

hooke2d_rel :: Relation
hooke2d_rel = vec2D (inxi genPressure) (inxi genPressure) := dgnl2x2 (inxi shrStiffIntsl) (inxi nrmStiffBase) * vec2D (inxi dx_i) (inxi dy_i)

hooke2d_desc :: Sentence
hooke2d_desc = foldlSent [S "A 2D component implementation of Hooke's law as seen in" +:+.
  acroT 5, getS elmPrllDispl, S "is", phrase displacement `ofThe` phrase element,
  S "normal to the", phrase surface, S "and", getS elmNrmDispl, S "is",
  phrase displacement `ofThe` phrase element, S "parallel to the" +:+. phrase surface,
  S "Pn,i is the net pressure acting normal to the", phrase surface `sC`
  S "and Pt,i is the net pressure acting parallel to the" +:+. phrase surface,
  S "Pressure is used in place of", phrase force, S "as the", phrase surface,
  S "has not been normalized for it's" +:+. phrase len, S "The stiffness", plural value,
  S "Kn,i and Kt,i are then the resistance to", phrase displacement,
  -- FIXME: Pn,i ~ Pt,i ~ Kn,i ~ Kt,i need symbols 
  S "in the respective directions defined as in" +:+. acroDD 14, S "The pressure",
  plural force, S "would be the result of applied loads on the", phrase mass `sC`
  S "the product of the stiffness", plural element, S "with the", phrase displacement,
  S "would be the", phrase mass, S "'s reactive", phrase force,
  S "that creates equilibrium with the applied", plural force,
  S "after reaching the equilibrium", phrase displacement]
  -- FIXME: way to give possessive attribute to noun (ex. "mass's")


--
displVect :: RelationConcept
displVect = makeRC "displVect" (nounPhraseSP "displacement vectors") disVec_desc disVec_rel

disVec_rel :: Relation
disVec_rel = inxi rotatedDispl := vec2D (inxi shrDispl) (inxi nrmDispl) :=
  m2x2 (cos(inxi baseAngle)) (sin(inxi baseAngle)) (Neg $ sin(inxi baseAngle)) (cos(inxi baseAngle)) *
  (inxi genDisplace) :=
  m2x2 (cos(inxi baseAngle)) (sin(inxi baseAngle)) (Neg $ sin(inxi baseAngle)) (cos(inxi baseAngle)) *
  vec2D (inxi dx_i) (inxi dy_i)

disVec_desc :: Sentence
disVec_desc = foldlSent [at_start' vector, S "describing the", phrase displacement,
  S "of", phrase slice +:+. S "i", getS genDisplace `isThe`
  phrase displacement, S "in the unrotated coordinate system" `sC`
  S "where", getS dx_i `isThe` phrase displacement, S "of the", phrase slice,
  phrase perp, S "to the direction of gravity, and", getS dy_i `isThe`
  phrase displacement, S "of the", phrase slice, S "parallel to the", 
  phrase force +:+. S "of gravity", getS rotatedDispl `isThe`
  phrase displacement, S "in the rotated coordinate", phrase system `sC`
  S "where", getS shrDispl `isThe` phrase displacement, S "of the",
  phrase slice, S "parallel to the", phrase slice, S "base, and", 
  getS dy_i `isThe` phrase displacement, S "of the", phrase slice,
  S "perpendicular to the", phrase slice +:+. S "base", getS rotatedDispl,
  S "can also be found by rotating", getS genDisplace,
  S "clockwise by the base", phrase angle `sC` getS baseAngle,
  S "through a rotation", phrase matrix, S "as shown"]
  --FIXME: some symbols need to be vectors and indexed
