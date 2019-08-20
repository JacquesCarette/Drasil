module Drasil.SSP.TMods (tMods, factOfSafety, equilibrium, mcShrStrgth, effStress) 
  where

import Prelude hiding (tan)
import Language.Drasil
import Theory.Drasil (TheoryModel, tm)
import Utils.Drasil

import Data.Drasil.Quantities.Physics (distance, force)

import Data.Drasil.Concepts.Documentation (model)
import Data.Drasil.Concepts.Physics (friction, linear)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.SSP.Assumptions (assumpENSL, assumpSBSBISL)
import Drasil.SSP.Defs (factorOfSafety)
import Drasil.SSP.References (fredlund1977)
import Drasil.SSP.Unitals (effCohesion, effNormStress, effectiveStress, 
  fricAngle, fs, fx, fy, mobilizedShear, momntOfBdy, nrmFSubWat, porePressure, 
  resistiveShear, shrStress, totStress)
import Drasil.SSP.DataDefs (stressDD)

--------------------------
--  Theoretical Models  --
--------------------------
tMods :: [TheoryModel]
tMods = [factOfSafety, equilibrium, mcShrStrgth, effStress, newtonSL]

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm (cw factOfSafetyRC)
  [qw fs, qw resistiveShear, qw mobilizedShear] ([] :: [ConceptChunk])
  [] [factOfSafetyRel] [] [makeCite fredlund1977] "factOfSafety" []

------------------------------------
factOfSafetyRC :: RelationConcept
factOfSafetyRC = makeRC "factOfSafetyRC" factorOfSafety EmptyS factOfSafetyRel

factOfSafetyRel :: Relation
factOfSafetyRel = sy fs $= sy resistiveShear / sy mobilizedShear

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm (cw equilibriumRC)
  [qw fx] ([] :: [ConceptChunk])
  [] [eqRel] [] [makeCite fredlund1977] "equilibrium" [eqDesc]

------------------------------------  
equilibriumRC :: RelationConcept
equilibriumRC = makeRC "equilibriumRC" (nounPhraseSP "equilibrium") eqDesc eqRel

-- FIXME: Variable "i" is a hack.  But we need to sum over something!
eqRel :: Relation
eqRel = foldr (($=) . sumAll (Variable "i") . sy) 0 [fx, fy, momntOfBdy]

eqDesc :: Sentence
eqDesc = foldlSent [S "For a body in static equilibrium, the net",
  plural force, S "and", plural momntOfBdy +:+. S "acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (makeRef2S assumpENSL) `sC` S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net", phrase momntOfBdy, S "equal to" +:+ E 0]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm (cw mcShrStrgthRC)
  [qw shrStress, qw effNormStress, qw fricAngle, qw effCohesion] 
  ([] :: [ConceptChunk])
  [] [mcShrStrgthRel] [] [makeCite fredlund1977] "mcShrStrgth" [mcShrStrgthDesc]

------------------------------------
mcShrStrgthRC :: RelationConcept
mcShrStrgthRC = makeRC "mcShrStrgthRC" (nounPhraseSP "Mohr-Coulumb shear strength")
  mcShrStrgthDesc mcShrStrgthRel

mcShrStrgthRel :: Relation
mcShrStrgthRel = sy shrStress $= (sy effNormStress * tan (sy fricAngle) + sy effCohesion)

mcShrStrgthDesc :: Sentence
mcShrStrgthDesc = foldlSent [S "In this", phrase model, S "the",
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
effStress = tm (cw effStressRC)
  [qw effectiveStress, qw totStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [] [effStressRel] [] [makeCite fredlund1977] "effStress" [effStressDesc]

------------------------------------
effStressRC :: RelationConcept
effStressRC = makeRC "effStressRC"
  (nounPhraseSP "effective stress") effStressDesc effStressRel -- l4

effStressRel :: Relation
effStressRel = sy effectiveStress $= sy totStress - sy porePressure

effStressDesc :: Sentence
effStressDesc = foldlSent [ch totStress, S "is defined in", makeRef2S stressDD]