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

import Drasil.SSP.Assumptions (assumpENSL, assumpSBSBISL)
import Drasil.SSP.Defs (factor, factorOfSafety, slope, soil)
import Drasil.SSP.References (fredlund1977)
import Drasil.SSP.Unitals (effCohesion, effNormStress, effectiveStress, fricAngle, fs, fx, fy, mobShrI,
  momntOfBdy, nrmFSubWat, porePressure, shrResI, shrStress, surfHydroForce, totStress)
import Drasil.SSP.DataDefs (stressDD)

--------------------------
--  Theoretical Models  --
--------------------------

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm (cw factOfSafety_rc)
  [qw fs, qw shrResI, qw mobShrI] ([] :: [ConceptChunk])
  [] [factOfSafety_rel] [] [makeCite fredlund1977] "factOfSafety" []

------------------------------------
factOfSafety_rc :: RelationConcept
factOfSafety_rc = makeRC "factOfSafety_rc" factorOfSafety EmptyS factOfSafety_rel

factOfSafety_rel :: Relation
factOfSafety_rel = (sy fs) $= (sy shrResI) / (sy mobShrI)

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm (cw equilibrium_rc)
  [qw fx] ([] :: [ConceptChunk])
  [] [eq_rel] [] [makeCite fredlund1977] "equilibrium" [eq_desc]

------------------------------------  
equilibrium_rc :: RelationConcept
equilibrium_rc = makeRC "equilibrium_rc" (nounPhraseSP "equilibrium") eq_desc eq_rel

-- FIXME: Atomic "i" is a hack.  But we need to sum over something!
eq_rel :: Relation
eq_rel = foldr (($=) . sum_all (Atomic "i") . sy) 0 [fx, fy, momntOfBdy]

eq_desc :: Sentence
eq_desc = foldlSent [S "For a body in static equilibrium, the net",
  plural force, S "and", plural momntOfBdy +:+. S "acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (makeRef2S assumpENSL) `sC` S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a", phrase momntOfBdy, S "equal to" +:+ E 0]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm (cw mcShrStrgth_rc)
  [qw shrStress, qw effNormStress, qw fricAngle, qw effCohesion] 
  ([] :: [ConceptChunk])
  [] [mcSS_rel] [] [makeCite fredlund1977] "mcShrStrgth" [mcSS_desc]

------------------------------------
mcShrStrgth_rc :: RelationConcept
mcShrStrgth_rc = makeRC "mcShrStrgth_rc" (nounPhraseSP "Mohr-Coulumb shear strength")
  mcSS_desc mcSS_rel

mcSS_rel :: Relation
mcSS_rel = (sy shrStress) $= ((sy effNormStress) * (tan (sy fricAngle)) + (sy effCohesion))

mcSS_desc :: Sentence
mcSS_desc = foldlSent [S "In this", phrase model, S "the",
  getTandS shrStress, S "is proportional to the product of the",
  phrase effNormStress, ch effNormStress, S "on the plane", 
  S "with its static", phrase friction, S "in the angular form" +:+.
  (E $ tan (sy fricAngle)),
  S "The", ch shrStress, S "versus", ch effNormStress,
  S "relationship is not truly",
  phrase linear `sC` S "but assuming the", phrase nrmFSubWat, 
  S "is strong enough, it can be approximated with a", phrase linear,
  S "fit", sParen (makeRef2S assumpSBSBISL), S "where the", phrase effCohesion, 
  ch effCohesion, S "represents the", ch shrStress,
  S "intercept of the fitted line"]

--
------------- New Chunk -----------
effStress :: TheoryModel
effStress = tm (cw effStress_rc)
  [qw effectiveStress, qw totStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [] [effS_rel] [] [makeCite fredlund1977] "effStress" [effS_desc]

------------------------------------
effStress_rc :: RelationConcept
effStress_rc = makeRC "effStress_rc"
  (nounPhraseSP "effective stress") EmptyS effS_rel -- l4

effS_rel :: Relation
effS_rel = (sy effectiveStress) $= (sy totStress) - (sy porePressure)

effS_desc :: Sentence
effS_desc = foldlSent [ch totStress, S "is defined in", makeRef2S stressDD]