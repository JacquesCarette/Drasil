module Drasil.SSP.TMods (tMods, factOfSafety, equilibrium, mcShrStrgth, effStress) 
  where

import Prelude hiding (tan)
import Language.Drasil
import Theory.Drasil (TheoryModel, tm, ModelKinds(EquationalModel, OthModel))
import Utils.Drasil

import Data.Drasil.Quantities.Physics (distance, force)

import Data.Drasil.Concepts.Documentation (model)
import Data.Drasil.Concepts.Physics (friction, linear)
import Data.Drasil.Theories.Physics (newtonSL)

import Drasil.SSP.Assumptions (assumpENSL, assumpSBSBISL)
import Drasil.SSP.Defs (factorOfSafety)
import Drasil.SSP.References (fredlund1977)
import Drasil.SSP.Unitals (effCohesion, effNormStress, effectiveStress, 
  fricAngle, fs, fx, fy, genericM, mobilizedShear, nrmFSubWat, porePressure, 
  resistiveShear, shrStress, totNormStress)
import Drasil.SSP.DataDefs (normStressDD)

--------------------------
--  Theoretical Models  --
--------------------------
tMods :: [TheoryModel]
tMods = [factOfSafety, equilibrium, mcShrStrgth, effStress, newtonSL]

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm (EquationalModel factOfSafetyQD)
  [qw fs, qw resistiveShear, qw mobilizedShear] ([] :: [ConceptChunk])
  [] [relat factOfSafetyQD] [] [makeCite fredlund1977] "factOfSafety" []

------------------------------------
factOfSafetyQD :: QDefinition
factOfSafetyQD = mkQuantDef' fs factorOfSafety factOfSafetyExpr

factOfSafetyExpr :: Expr
factOfSafetyExpr = sy resistiveShear / sy mobilizedShear

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm (OthModel equilibriumRC)
  [qw fx] ([] :: [ConceptChunk])
  [] [eqRel] [] [makeCite fredlund1977] "equilibrium" [eqDesc]

------------------------------------  
equilibriumRC :: RelationConcept
equilibriumRC = makeRC "equilibriumRC" (nounPhraseSP "equilibrium") eqDesc eqRel

-- FIXME: Variable "i" is a hack.  But we need to sum over something!
eqRel :: Relation
eqRel = foldr (($=) . sumAll (Variable "i") . sy) 0 [fx, fy, genericM]

eqDesc :: Sentence
eqDesc = foldlSent [S "For a body in static equilibrium, the net",
  plural force, S "and", plural genericM +:+. S "acting on the body will cancel out",
  S "Assuming a 2D problem", sParen (makeRef2S assumpENSL) `sC` S "the", getTandS fx `sAnd`
  getTandS fy, S "will be equal to" +:+. E 0, S "All", plural force,
  S "and their", phrase distance, S "from the chosen point of rotation",
  S "will create a net", phrase genericM, S "equal to" +:+ E 0]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm (OthModel mcShrStrgthRC)
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
  E (tan $ sy fricAngle),
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
effStress = tm (OthModel effStressRC)
  [qw effectiveStress, qw totNormStress, qw porePressure] 
  ([] :: [ConceptChunk])
  [] [effStressRel] [] [makeCite fredlund1977] "effStress" [effStressDesc]

------------------------------------
effStressRC :: RelationConcept
effStressRC = makeRC "effStressRC"
  (nounPhraseSP "effective stress") effStressDesc effStressRel -- l4

effStressRel :: Relation
effStressRel = sy effectiveStress $= sy totNormStress - sy porePressure

effStressDesc :: Sentence
effStressDesc = foldlSent [ch totNormStress, S "is defined in", makeRef2S normStressDD]