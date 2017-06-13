module Drasil.SSP.GenDefs where

import Prelude hiding (tan)

import Language.Drasil
import Drasil.SSP.Units
import Data.Drasil.Concepts.Documentation
import Drasil.SSP.Defs
import Data.Drasil.Concepts.PhysicalProperties
import Data.Drasil.Concepts.Physics
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
  normShrR, momentEql, netForce, hooksLaw2d, displVect]

fixmeS :: Sentence
fixmeS = S "FIXME: add description"
--
normForcEq :: RelationConcept
normForcEq = makeRC "normForcEq" (nounPhraseSP "normal force equilibrium") nmFEq_desc nmFEq_rel

nmFEq_rel :: Relation
nmFEq_rel = C totNrmForce := (Int 0) --FIXME: add the long equation

nmFEq_desc :: Sentence
nmFEq_desc = S "For a" +:+ phrase slice +:+ S "of" +:+ phrase mass +:+
  S "in the" +:+ phrase slope +:+ S "the" +:+ phrase force +:+
  S "equilibrium to satisfy" +:+ acroT "2" +:+ S "in the direction" +:+
  S "perpendicular to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice) +:+ S "Rearranged to solve for" +:+ (phrase normForce `ofThe`
  phrase surface) +:+. getS totNrmForce +:+ at_start force +:+ S "equilibrium is" +:+
  S "derived from the free body diagram of" +:+
  makeRef (SRS.physSyst SRS.missingP []) +:+ S "Index i" +:+
  S "refers to" +:+ (plural value `ofThe` plural property) +:+ S "for" +:+
  phrase slice :+: S "/" :+: plural intrslce +:+ S "following convention in" +:+.
  makeRef (SRS.physSyst SRS.missingP []) +:+ at_start force +:+ phrase variable +:+
  plural definition +:+ S "can be found in" +:+ acroDD "1" +:+ S "to" +:+.
  acroDD "9"

--
bsShrFEq :: RelationConcept
bsShrFEq = makeRC "bsShrFEq" (nounPhraseSP "base shear force equilibrium") bShFEq_desc bShFEq_rel

bShFEq_rel :: Relation
bShFEq_rel = C mobShrI := (Int 0) --FIXME: add the long equation

bShFEq_desc :: Sentence
bShFEq_desc = S "For a" +:+ phrase slice +:+ S "of" +:+ phrase mass +:+
  S "in the" +:+ phrase slope +:+ S "the" +:+ phrase force +:+
  S "equilibrium to satisfy" +:+ acroT "2" +:+ S "in the direction" +:+
  S "parallel to" +:+. (S "base" +:+ phrase surface `ofThe`
  phrase slice) +:+ S "Rearranged to solve for the" +:+ phrase shearForce +:+
  S "on the base" +:+. getS mobShrI +:+ at_start force +:+ S "equilibrium is" +:+
  S "derived from the free body diagram of" +:+
  makeRef (SRS.physSyst SRS.missingP []) +:+ S "Index i" +:+
  S "refers to" +:+ (plural value `ofThe` plural property) +:+ S "for" +:+
  phrase slice :+: S "/" :+: plural intrslce +:+ S "following convention in" +:+.
  makeRef (SRS.physSyst SRS.missingP []) +:+ at_start force +:+ phrase variable +:+
  plural definition +:+ S "can be found in" +:+ acroDD "1" +:+ S "to" +:+.
  acroDD "9"

--
resShr :: RelationConcept
resShr = makeRC "resShr" (nounPhraseSP "resistive shear") resShr_desc resShr_rel

resShr_rel :: Relation
resShr_rel = C shrResI := C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)

resShr_desc :: Sentence
resShr_desc = S "The Mohr-Coulomb resistive shear strength of a" +:+
  phrase slice +:+ getS shrResI +:+ S "is adjusted to account for the" +:+
  S "effective normal" +:+
  getS nrmStrss :+: S "'" +:+ -- FIXME: Need to add prime without hardcoding
  S "=" +:+ S "N' = N -Ub of a soil from" +:+. acroT "4" +:+
  S "Also and the cohesion is" +:+
  S "adjusted to account for the length l of the plane where the normal occurs" `sC`
  S "where lb,i = bi * sec (alpha), and bi is the x width of the base." +:+
  S "Therefore c=c' *bi *sec(alpha*i)."
  -- FIXME: Still needs to be more automated

--
mobShr :: RelationConcept
mobShr = makeRC "mobShr" (nounPhraseSP "mobile shear") mobShr_desc mobShr_rel

mobShr_rel :: Relation
mobShr_rel = C mobShrI := C shrResI :/ C fs := 
  (C nrmFSubWat :* tan (C fricAngle) :+ C cohesion :* C baseWthX :* sec (C baseAngle)) :/ C fs

mobShr_desc :: Sentence
mobShr_desc = foldlSent [S "From the definition of the Factor of Safety in T1" `sC`
  S "and the new definition of", getS shrResI `sC` S "a new relation for the net mobile", 
  S "shear force of the slice", getS ti, S "is found as the resistive shear" , getS shrResI,
  S "(GD3) divided by the factor of safety", getS fs]

--
normShrR :: RelationConcept
normShrR = makeRC "normShrR" (nounPhraseSP "interslice normal/shear relationship") nmShrR_desc nmShrR_rel

nmShrR_rel :: Relation
nmShrR_rel = C intShrForce := C normToShear :* C scalFunc :* C intNormForce

nmShrR_desc :: Sentence
nmShrR_desc = foldlSent [S "The assumption for the Morgenstern Price method (A5)",
  S "that the interslice shear force", getS xi, S "is proportional to the interslice", 
  S "normal force", getS intNormForce, S "by a proportionality constant",
  (P $ (Greek Lambda_L)), S "and a predetermined scaling function", P (lF) `sC` S "that", 
  S "changes the proportionality as a function of the x-ordinate position of the interslice.",
  P (lF), S "is typically either a half-sine along the slip surface, or a constant"]

--
momentEql :: RelationConcept
momentEql = makeRC "momentEql" (nounPhraseSP "moment equilibrium") momEql_desc momEql_rel

momEql_rel :: Relation
momEql_rel = (Int 0) := (Int 0) --FIXME: add the long equation

momEql_desc :: Sentence
momEql_desc = foldlSent []

--
netForce :: RelationConcept
netForce = makeRC "netForce" (nounPhraseSP "net force") fNet_desc fNet_rel

fNet_rel :: Relation
fNet_rel = C genForce := (Int 0) --FIXME: requires two lines of equal signs

fNet_desc :: Sentence
fNet_desc = foldlSent [S "The net sum of forces acting on a slice for",
  S "the RFEM model. The forces that create an applied load on the slice.", 
  S "Fx,i refers to the load in the direction perpendicular to the", 
  S "direction of the force of gravity for slice i, while Fy,i refers", 
  S "to the load in the direction parallel to the force of gravity for", 
  S "slice i. Forces are found in the free body diagram of Fig2 in section", 
  S "4.1.2. In this model the elements are not exerting forces on each other" `sC`
  S "so the interslice forces E and X are not a part of the model. Index i", 
  S "refers to the values of the properties for slice/interslices following", 
  S "convention in Fig1 in section 4.1.2. Force variable definitions can",
  S "be found in DD1 to DD8"]
  --FIXME:Finish pulling out symbols

--
hooksLaw2d :: RelationConcept
hooksLaw2d = makeRC "hooksLaw2d" (nounPhraseSP "Hook's law 2D") hook2d_desc hook2d_rel

hook2d_rel :: Relation
hook2d_rel = (Int 0) := (Int 0) --FIXME: cannot yet generate matricies

hook2d_desc :: Sentence
hook2d_desc = fixmeS

--
displVect :: RelationConcept
displVect = makeRC "displVect" (nounPhraseSP "displacement vectors") disVec_desc disVec_rel

disVec_rel :: Relation
disVec_rel = (Int 0) := (Int 0) --FIXME: cannot yet generate matricies

disVec_desc :: Sentence
disVec_desc = fixmeS