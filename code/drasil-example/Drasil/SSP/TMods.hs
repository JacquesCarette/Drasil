module Drasil.SSP.TMods (factOfSafety, equilibrium, mcShrStrgth, effStress) 
  where

import Prelude hiding (tan)
import Language.Drasil

import Data.Drasil.Quantities.Physics (distance, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.SolidMechanics (mobShear, shearRes)

import Data.Drasil.Concepts.Documentation (model, safety, source)
import Data.Drasil.Concepts.Math (surface)
import Data.Drasil.Concepts.Physics (friction, linear, stress)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)

import Data.Drasil.SentenceStructures (foldlSent, getTandS, ofThe, ofThe',
  sAnd, sOf)

import Drasil.SSP.Assumptions (newA8, newA9)
import Drasil.SSP.Defs (factor, factorOfSafety, slope, soil)
import Drasil.SSP.References (fredlund1977)
import Drasil.SSP.Unitals (cohesion, fricAngle, fs, fx, fy,
  momntOfBdy, normStress, porePressure, shrStress, surfHydroForce)

-- Pre-defined some labels. They will be re-used for tings which are 'the same'
l1, l2, l3, l4 :: Label
l1 = mkLabelSame "factOfSafety" (Def TM)
l2 = mkLabelSame "equilibrium"  (Def TM)
l3 = mkLabelSame "mcShrStrgth"  (Def TM)
l4 = mkLabelSame "effStress"    (Def TM)

--------------------------
--  Theoretical Models  --
--------------------------

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm' (cw factOfSafety_rc)
  (tc' "factOfSafety" [qw fs, qw shearRes, qw mobShear] ([] :: [ConceptChunk])
  [] [factOfSafety_rel] [] [makeRef fredlund1977]) l1 [factOfSafety_desc]

------------------------------------
factOfSafety_rc :: RelationConcept
factOfSafety_rc = makeRC "factOfSafety_rc" factorOfSafety factOfSafety_desc factOfSafety_rel l1

factOfSafety_rel :: Relation
factOfSafety_rel = (sy fs) $= (sy shearRes) / (sy mobShear)

factOfSafety_desc :: Sentence
factOfSafety_desc = foldlSent [
  S "The stability metric of the", phrase slope `sC` S "known as the",
  phrase factor `sOf` phrase safety, sParen (ch fs) `sC`
  S "is determined by", S "ratio" `ofThe` phrase shearForce,
  S "at the base of the", phrase slope, sParen (ch mobShear) `sC`
  S "and the resistive shear", sParen (ch shearRes)]

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm' (cw equilibrium_rc)
  (tc' "equilibrium" [qw fx] ([] :: [ConceptChunk])
  [] [eq_rel] [] [makeRef fredlund1977]) l2 [eq_desc]

------------------------------------  
equilibrium_rc :: RelationConcept
equilibrium_rc = makeRC "equilibrium_rc" (nounPhraseSP "equilibrium") eq_desc eq_rel l2

-- FIXME: Atomic "i" is a hack.  But we need to sum over something!
eq_rel :: Relation
eq_rel = foldr ($=) 0 (map summ [fx, fy, momntOfBdy])
  where summ = sum_all (Atomic "i") . sy

eq_desc :: Sentence
eq_desc = foldlSent [S "For a body in static equilibrium, the net",
  plural force +:+. S "and net moments acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (makeRefS newA8), S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net moment equal to" +:+ E 0]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm' (cw mcShrStrgth_rc)
  (tc' "mcShrStrgth" [qw shrStress, qw normStress, qw fricAngle, qw cohesion] 
  ([] :: [ConceptChunk])
  [] [mcSS_rel] [] [makeRef fredlund1977]) l3 [mcSS_desc]

------------------------------------
mcShrStrgth_rc :: RelationConcept
mcShrStrgth_rc = makeRC "mcShrStrgth_rc" (nounPhraseSP "Mohr-Coulumb shear strength")
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
  S "fit", sParen (makeRefS newA9), S "where the cohesion", ch cohesion,
  S "represents the", ch shrStress, S "intercept of the fitted line"]

--
------------- New Chunk -----------
effStress :: TheoryModel
effStress = tm' (cw effStress_rc)
  (tc' "effStress" [qw normStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [] [effS_rel] [] [makeRef fredlund1977]) l4 [effS_desc]

------------------------------------
effStress_rc :: RelationConcept
effStress_rc = makeRC "effStress_rc"
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
