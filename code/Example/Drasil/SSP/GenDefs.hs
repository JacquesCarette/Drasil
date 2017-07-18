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

---------------------------
--  General Definitions  --
---------------------------

sspGenDefs :: [RelationConcept]
sspGenDefs = [normForcEq, bsShrFEq, resShr, mobShr,
  normShrR, momentEql, netForce, hookesLaw2d, displVect]

--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium") nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = C totNrmForce := ((C slcWght :- C intShrForce :+ C intShrForce :+ 
                          C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
                          cos (C impLoadAngle)) :* cos (C baseAngle)
                          :+ (Neg (C earthqkLoadFctr) :* C slcWght :- 
                          C intNormForce :+ C intNormForce :- C watrForce :+ 
                          C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
                          C surfLoad :* cos (C impLoadAngle)) :* sin (C baseAngle)) -- FIXME: add the proper index for intShrForce and intNormForce

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
bShFEq_rel = C mobShrI := ((C slcWght :- C intShrForce :+ C intShrForce :+ 
                          C baseHydroForce :* cos (C surfAngle) :+ C surfLoad :* 
                          cos (C impLoadAngle)) :* sin (C baseAngle)
                          :- (Neg (C earthqkLoadFctr) :* C slcWght :- 
                          C intNormForce :+ C intNormForce :- C watrForce :+ 
                          C watrForce :+ C surfHydroForce :* sin (C surfAngle) :+ 
                          C surfLoad :* cos (C impLoadAngle)) :* cos (C baseAngle)) -- FIXME: add the proper index for intShrForce and intNormForce

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
resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear force") resShr_desc resShr_rel

resShr_rel :: Relation
resShr_rel = C shrResI := C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)

resShr_desc :: Sentence
resShr_desc = foldlSent [S "The Mohr-Coulomb resistive shear strength of a",
  phrase slice, getS shrStress, S "from", acroT 3, S "is multiplied by the area",
  E $ C baseWthX * sec(C baseAngle) * Int 1, S "to obtain the" +:+. getTandS shrResI,
  S "Note the extra", E $ Int 1 , S "is to represent a unit of width which is",
  S "multiplied by the", getTandS baseLngth, S "of the plane where the",
  phrase normal, S "occurs, where", (E $ C baseLngth := C baseWthX * sec(C baseAngle))
  `sAnd` getS baseWthX, S "is the x width of the base. This accounts for the",
  phrase nrmFSubWat, E $ C nrmFSubWat := C totNrmForce - C baseHydroForce, S "of a soil from", -- FIXME: add prime to nrmStrss
  acroT 4, S "where the", phrase nrmStrss, S "is multiplied by the same area to obtain the",
  phrase nrmFSubWat, E $ C nrmStrss * C baseWthX * sec(C baseAngle) * Int 1 := C nrmFSubWat]

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr" (nounPhraseSP "mobile shear force") mobShr_desc mobShr_rel

mobShr_rel :: Relation
mobShr_rel = C mobShrI := C shrResI :/ C fs := 
  (C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)) :/ C fs

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
momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium") momEql_desc momEql_rel

momEql_rel :: Relation
momEql_rel = (Int 0) := Neg (C intNormForce) :* (Int 1 :- C baseWthX :/ Int 2 :* 
                        tan (C baseAngle)) :+ C intNormForce :* (Int 1 :- 
                        C baseWthX :/ Int 2 :* tan (C baseAngle)) :- C watrForce :*
                        (Int 1 :- C baseWthX :/ Int 2 :* tan (C baseAngle)) :+ 
                        C watrForce :* (Int 1 :- C baseWthX :/ Int 2 :* 
                        tan (C baseAngle)) :- C baseWthX :/ Int 2 :* 
                        (C intShrForce :+ C intShrForce) :+ C earthqkLoadFctr :* 
                        C slcWght :* C midpntHght :/ Int 2 :- C surfHydroForce :*
                        sin (C surfAngle) :* C midpntHght :- C surfLoad :* 
                        sin (C impLoadAngle) :* C midpntHght -- FIXME: replace Int 1 with zi and add the proper index for zi, bi, Ei, Hi and Xi

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
netForce :: RelationConcept
netForce = makeRC "netForce" (nounPhraseSP "net force") fNet_desc fNet_rel

fNet_rel :: Relation
fNet_rel = C genForce := (Neg $ C watrForceDif) - (C earthqkLoadFctr)*(C slcWght)
  - (C baseHydroForce) * sin (C baseAngle) + (C surfHydroForce) * sin (C surfAngle)
  + (C surfLoad) * sin (C impLoadAngle)
{-
C genForce := (Neg $ C slcWght) + (C baseHydroForce) * cos (C baseAngle) - (C surfHydroForce) * cos (C surfAngle)
  - (C surfLoad) * cos (C impLoadAngle)
-}
--FIXME: requires two lines of equal signs

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
hooke2d_rel = vec2D (C genPressure) (C genPressure) := dgnl2x2 (C shrStiffIntsl) (C nrmStiffBase) * vec2D (C dx_i) (C dy_i)

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
disVec_rel = C rotatedDispl := vec2D (C shrDispl) (C nrmDispl) :=
  m2x2 (cos(C baseAngle)) (sin(C baseAngle)) (Neg $ sin(C baseAngle)) (cos(C baseAngle)) *
  (C genDisplace) :=
  m2x2 (cos(C baseAngle)) (sin(C baseAngle)) (Neg $ sin(C baseAngle)) (cos(C baseAngle)) *
  vec2D (C dx_i) (C dy_i)

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
