{-# LANGUAGE PostfixOperators #-}
module Drasil.SSP.TMods (tMods, factOfSafety, equilibrium, mcShrStrgth, effStress)
  where

import Control.Lens ((^.))
import Prelude hiding (tan)
import qualified Data.List.NonEmpty as NE

import Drasil.Database (HasUID(..))
import Language.Drasil
import Theory.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Drasil.Sentence.Combinators (definedIn''')

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

------------------------
-- Theoretical Models --
------------------------
tMods :: [TheoryModel]
tMods = [factOfSafety, equilibrium, mcShrStrgth, effStress, newtonSL]

------------- New Chunk -----------
factOfSafety :: TheoryModel
factOfSafety = tm (equationalModelU "factOfSafetyTM" factOfSafetyQD)
  ([] :: [DefinedQuantityDict]) ([] :: [ConceptChunk])
  [factOfSafetyQD] [] [] [dRef fredlund1977] "factOfSafety" []

------------------------------------
factOfSafetyQD :: ModelQDef
factOfSafetyQD = mkQuantDef' fs factorOfSafety factOfSafetyExpr

factOfSafetyExpr :: PExpr
factOfSafetyExpr = sy resistiveShear $/ sy mobilizedShear

--
------------- New Chunk -----------
equilibrium :: TheoryModel
equilibrium = tm (equationalConstraints' equilibriumCS)
  ([] :: [DefinedQuantityDict]) ([] :: [ConceptChunk])
  [] (map express equilibriumRels) [] [dRef fredlund1977] "equilibrium" [eqDesc]

------------------------------------

equilibriumRels :: [ModelExpr]
equilibriumRels = map (($= int 0) . sumAll (variable "i") . sy) [fx, fy, genericM]

-- FIXME: variable "i" is a hack. But we need to sum over something!
equilibriumCS :: ConstraintSet ModelExpr
equilibriumCS = mkConstraintSet
  (dccWDS "equilibriumCS" (nounPhraseSP "equilibrium") eqDesc) $
  NE.fromList equilibriumRels
-- makeRC "equilibriumRC" (nounPhraseSP "equilibrium") eqDesc eqRel

eqDesc :: Sentence
eqDesc = foldlSent [S "For a body in static equilibrium" `sC` S "the net",
  D.toSent (pluralNP (force `and_PP` genericM)) +:+. (S "acting" `S.onThe` S "body will cancel out"),
  S "Assuming a 2D problem", sParen (refS assumpENSL) `sC` S "the", getTandS fx `S.and_`
  getTandS fy, S "will be equal to" +:+. eS (exactDbl 0), S "All", plural force,
  S "and their", phrase distance, S "from the chosen point" `S.of_` S "rotation",
  S "will create a net", phrase genericM, S "equal to" +:+ eS (exactDbl 0)]

--
------------- New Chunk -----------
mcShrStrgth :: TheoryModel
mcShrStrgth = tm (equationalModelU "mcShrSrgth" mcShrStrgthQD)
  ([] :: [DefinedQuantityDict])
  ([] :: [ConceptChunk])
  [mcShrStrgthQD] [] [] [dRef fredlund1977] "mcShrStrgth" [mcShrStrgthDesc]

------------------------------------
mcShrStrgthQD :: ModelQDef
mcShrStrgthQD = fromEqnSt' (shrStress ^. uid) (nounPhraseSP "Mohr-Coulumb shear strength")
 mcShrStrgthDesc (symbol shrStress) Real mcShrStrgthExpr

mcShrStrgthExpr :: PExpr
mcShrStrgthExpr = sy effNormStress $* tan (sy fricAngle) $+ sy effCohesion

mcShrStrgthDesc :: Sentence
mcShrStrgthDesc = foldlSent [S "In this", phrase model, S "the",
  getTandS shrStress `S.is` S "proportional to the product" `S.ofThe` phrase effNormStress,
  ch effNormStress `S.onThe` S "plane",
  S "with its static", phrase friction `S.inThe` S "angular form" +:+.
  eS (tan $ sy fricAngle),
  S "The", ch shrStress, S "versus", ch effNormStress,
  S "relationship" `S.is` S "not truly",
  phrase linear `sC` S "but assuming the", phrase nrmFSubWat,
  S "is strong enough" `sC` S "it can be approximated with a", phrase linear,
  S "fit", sParen (refS assumpSBSBISL), S "where the", phrase effCohesion,
  ch effCohesion, S "represents the", ch shrStress,
  S "intercept" `S.ofThe` S "fitted line"]

--
------------- New Chunk -----------
effStress :: TheoryModel
effStress = tm (equationalModelU "effectiveStressTM" effStressQD)
  ([] :: [DefinedQuantityDict])
  ([] :: [ConceptChunk])
  [effStressQD] [] [] [dRef fredlund1977] "effStress" [effStressDesc]

------------------------------------
effStressQD :: ModelQDef
effStressQD = fromEqnSt' (effectiveStress ^. uid) (nounPhraseSP "effective stress")
 effStressDesc (symbol effectiveStress) Real effStressExpr

effStressExpr :: PExpr
effStressExpr = sy totNormStress $- sy porePressure

effStressDesc :: Sentence
effStressDesc = (totNormStress `definedIn'''` normStressDD !.)
