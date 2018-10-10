module Drasil.GamePhysics.Changes where

--A list of likely and unlikely changes for GamePhysics

import Language.Drasil
import Drasil.DocLang (mkEnumSimpleD)
import qualified Drasil.DocLang.SRS as SRS (likeChg, unlikeChg)

import Data.Drasil.SentenceStructures (foldlSent, foldlSP, maybeChanged,
  maybeExpanded, sAnd)
import Data.Drasil.Concepts.Documentation (section_, likelyChg, unlikelyChg,
  physics, game, library, likeChgDom, unlikeChgDom)
import qualified Data.Drasil.Concepts.Math as CM (ode, constraint)
import Data.Drasil.Concepts.Computation (algorithm)
import qualified Data.Drasil.Concepts.Physics as CP (collision, damping, joint)

---------------------
--  LIKELY CHANGES --
---------------------

likelyChanges :: Section
likelyChangesIntro :: Contents
likelyChangesList :: [Contents]

likelyChanges = SRS.likeChg ([likelyChangesIntro] ++ likelyChangesList) []

likelyChangesIntro = foldlSP [S "This", phrase section_, 
  S "lists the", plural likelyChg, S "to be made to the",
  phrase game, phrase physics, phrase library]

likelyChangesStmt1, likelyChangesStmt2, likelyChangesStmt3,
  likelyChangesStmt4 :: Sentence

--these statements look like they could be parametrized
likelyChangesStmt1 = (S "internal" +:+ (getAcc CM.ode) :+:
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  (phrase library)) `maybeChanged` (S "in the future")

likelyChangesStmt2 = (phrase library) `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+
  plural CP.collision)

likelyChangesStmt3 = (phrase library) `maybeExpanded` (
  S "to include motion with" +:+ (phrase CP.damping))

likelyChangesStmt4 = (phrase library) `maybeExpanded` (S "to include" +:+
  (plural CP.joint) `sAnd` (plural CM.constraint))

lcVODES, lcEC, lcID, lcIJC :: ConceptInstance

lcVODES = cic "lcVODES" likelyChangesStmt1 "Variable-ODE-Solver" likeChgDom
lcEC = cic "lcEC" likelyChangesStmt2 "Expanded-Collisions" likeChgDom
lcID = cic "lcID" likelyChangesStmt3 "Include-Dampening" likeChgDom
lcIJC = cic "lcIJC" likelyChangesStmt4 "Include-Joints-Constraints" likeChgDom

likelyChangesList' :: [ConceptInstance]
likelyChangesList' = [lcVODES, lcEC, lcID, lcIJC]

likelyChangesList = mkEnumSimpleD likelyChangesList'

--------------------------------
--UNLIKELY CHANGES --
--------------------------------

unlikelyChanges :: Section
unlikelyChangesIntro :: Contents

unlikelyChanges = SRS.unlikeChg ([unlikelyChangesIntro] ++ unlikelyChangesList) []

unlikelyChangesIntro = foldlSP [S "This", phrase section_, S "lists the",
  plural unlikelyChg, S "to be made to the", phrase game, phrase physics,
  phrase library]

unlikelyChangesStmt1, unlikelyChangesStmt2,
  unlikelyChangesStmt3, unlikelyChangesStmt4 :: Sentence

unlikelyChangesStmt1 = foldlSent [S "The goal of the system is to simulate",
  S "the interactions of rigid bodies"]
unlikelyChangesStmt2 = foldlSent [S "There will always be a source of input",
  S "data external to the software"]
unlikelyChangesStmt3 = foldlSent [S "A Cartesian Coordinate system is used"]
unlikelyChangesStmt4 = foldlSent [S "All objects are rigid bodies"]

ucSRB, ucEI, ucCCS, ucORB :: ConceptInstance

ucSRB = cic "ucSRB" unlikelyChangesStmt1 "Simulate-Rigid-Bodies" unlikeChgDom
ucEI = cic "ucEI" unlikelyChangesStmt2 "External-Input" unlikeChgDom
ucCCS = cic "ucCCS" unlikelyChangesStmt3 "Cartesian-Coordinate-System" unlikeChgDom
ucORB = cic "ucORB" unlikelyChangesStmt4 "Objects-Rigid-Bodies" unlikeChgDom
  
unlikelyChangesList' :: [ConceptInstance]
unlikelyChangesList' = [ucSRB, ucEI, ucCCS, ucORB]

unlikelyChangesList :: [Contents]
unlikelyChangesList = mkEnumSimpleD unlikelyChangesList'
