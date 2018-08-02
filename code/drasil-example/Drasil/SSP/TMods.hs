module Drasil.SSP.TMods (fs_rc_new, equilibrium_new, mcShrStrgth_new, hookesLaw_new
  , effStress_new) where

import Prelude hiding (tan)
import Language.Drasil
import Drasil.DocLang (refA)

import Drasil.SSP.Assumptions (newA8, newA9, sspRefDB)
import Drasil.SSP.Defs (factor, factorOfSafety, slope, soil)
import Drasil.SSP.Unitals (cohesion, fricAngle, fs, fx, fy, genDisplace,
  genForce, momntOfBdy, normStress, porePressure, shrStress, surfHydroForce)

import Data.Drasil.Quantities.Physics (displacement, distance, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.SolidMechanics (mobShear, shearRes, stffness)

import Data.Drasil.Concepts.Documentation (model, safety, source)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Physics (friction, linear, stress)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)

import Data.Drasil.SentenceStructures (foldlSent, getTandS, ofThe, ofThe',
  sAnd, sOf)

-- Pre-defined some labels. They will be re-used for tings which are 'the same'
l1, l2, l3, l4, l5 :: Label
l1 = mkLabelRA'' "fs_rc"
l2 = mkLabelRA'' "equilibrium"
l3 = mkLabelRA'' "mcShrStrgth"
l4 = mkLabelRA'' "effStress"
l5 = mkLabelRA'' "hookesLaw"

--------------------------
--  Theoretical Models  --
--------------------------

------------- New Chunk -----------
fs_rc_new :: TheoryModel
fs_rc_new = tm' (cw fs_rc)
  (tc' "fs_rc_new" [qw fs, qw shearRes, qw mobShear] ([] :: [ConceptChunk])
  [] [TCon Invariant fs_rel] []) l1 [fs_desc]

------------------------------------
fs_rc :: RelationConcept
fs_rc = makeRC "fs_rc" factorOfSafety fs_desc fs_rel l1

fs_rel :: Relation
fs_rel = (sy fs) $= (sy shearRes) / (sy mobShear)

fs_desc :: Sentence
fs_desc = foldlSent [
  S "The stability metric of the", phrase slope `sC` S "known as the",
  phrase factor `sOf` phrase safety, sParen (ch fs) `sC`
  S "is determined by", S "ratio" `ofThe` phrase shearForce,
  S "at the base of the", phrase slope, sParen (ch mobShear) `sC`
  S "and the resistive shear", sParen (ch shearRes)]

--
------------- New Chunk -----------
equilibrium_new :: TheoryModel
equilibrium_new = tm' (cw equilibrium)
  (tc' "equilibrium_new" [qw fx] ([] :: [ConceptChunk])
  [] [TCon Invariant eq_rel] []) l2 [eq_desc]

------------------------------------  
equilibrium :: RelationConcept
equilibrium = makeRC "equilibrium" (nounPhraseSP "equilibrium") eq_desc eq_rel l2

-- FIXME: Atomic "i" is a hack.  But we need to sum over something!
eq_rel :: Relation
eq_rel = foldr ($=) 0 (map summ [fx, fy, momntOfBdy])
  where summ = sum_all (Atomic "i") . sy

eq_desc :: Sentence
eq_desc = foldlSent [S "For a body in static equilibrium, the net",
  plural force +:+. S "and net moments acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (refA sspRefDB newA8), S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net moment equal to" +:+ E 0]

--
------------- New Chunk -----------
mcShrStrgth_new :: TheoryModel
mcShrStrgth_new = tm' (cw mcShrStrgth)
  (tc' "mcShrStrgth_new" [qw shrStress, qw normStress, qw fricAngle, qw cohesion] 
  ([] :: [ConceptChunk])
  [] [TCon Invariant mcSS_rel] []) l3 [mcSS_desc]

------------------------------------
mcShrStrgth :: RelationConcept
mcShrStrgth = makeRC "mcShrStrgth" (nounPhraseSP "Mohr-Coulumb shear strength")
  mcSS_desc mcSS_rel l3

mcSS_rel :: Relation
mcSS_rel = (sy shrStress) $= ((sy normStress) * (tan (sy fricAngle)) + (sy cohesion))

mcSS_desc :: Sentence
mcSS_desc = foldlSent [S "For a", phrase soil, S "under", phrase stress,
  S "it will exert a shear resistive strength based on the",
  S "Coulomb sliding law. The resistive shear is",
  S "the maximum amount of shear a", phrase surface,
  S "can experience while remaining rigid, analogous to",
  S "a maximum" +:+. phrase normForce, S "In this", phrase model, S "the",
  getTandS shrStress, S "is proportional to the product of the",
  phrase normStress, S "on the plane", ch normStress,
  S "with it's static", phrase friction, S "in the angular form" +:+.
  (E $ tan (sy fricAngle) $= sy surfHydroForce),
  --FIXME: sould say U_s but there is no way to say that yet
  S "The", ch shrStress, S "versus", ch normStress,
  S "relationship is not truly",
  phrase linear `sC` S "but assuming the effective", phrase normForce, 
  S "is strong enough, it can be approximated with a", phrase linear,
  S "fit", sParen (refA sspRefDB newA9), S "where the cohesion", ch cohesion,
  S "represents the", ch shrStress, S "intercept of the fitted line"]

--
------------- New Chunk -----------
effStress_new :: TheoryModel
effStress_new = tm' (cw effStress)
  (tc' "effStress_new" [qw normStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [] [TCon Invariant effS_rel] []) l4 [effS_desc]

------------------------------------
effStress :: RelationConcept
effStress = makeRC "effStress"
  (nounPhraseSP "effective stress") effS_desc effS_rel l4

effS_rel :: Relation
effS_rel = (sy normStress) $= (sy normStress) - (sy porePressure)

effS_desc :: Sentence --FIXME: these are not normStress but they are sigma.
                      -- Add a prime. Symbol inconsistency.
effS_desc = foldlSent [ch normStress, S "is the total", phrase stress,
  S "a soil", phrase mass,
  S "needs to maintain itself as a rigid collection of particles.",
  phrase source `ofThe'` phrase stress,
  S "can be provided by the soil skeleton", ch normStress `sC`
  S "or by the pore pressure from water within the soil" +:+.
  ch porePressure, S "The", phrase stress,
  S "from the soil skeleton is known as the effective",
  phrase stress, ch normStress, S "and is the difference between the",
  S "total", phrase stress, ch normStress, S "and the pore",
  phrase stress, ch porePressure]

--
------------- New Chunk -----------
hookesLaw_new :: TheoryModel
hookesLaw_new = tm' (cw hookesLaw)
  (tc' "effStress_new" [qw genForce, qw stffness, qw genDisplace] 
  ([] :: [ConceptChunk])
  [] [TCon Invariant hksLw_rel] []) l5 [hksLw_desc]

------------------------------------
hookesLaw :: RelationConcept
hookesLaw = makeRC "hookesLaw"
  (nounPhraseSP "Hooke's law") hksLw_desc hksLw_rel l5

hksLw_rel :: Relation
hksLw_rel = (sy genForce) $= (sy stffness) * (sy genDisplace)

hksLw_desc :: Sentence
hksLw_desc = foldlSent [S "Stiffness", ch stffness, S "is the",
  S "resistance of a body to deformation by", phrase displacement,
  ch genDisplace, S "when subject to a", phrase force, ch genForce `sC`
  S "along the same direction. A body with high stiffness will experience",
  S "little deformation when subject to a", phrase force]
