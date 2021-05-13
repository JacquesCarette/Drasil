{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.Changes (likelyChgs, unlikelyChgs) where

--A list of likely and unlikely changes for GamePhysics

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation as Doc (library, likeChgDom, unlikeChgDom)
import qualified Data.Drasil.Concepts.Math as CM (ode, constraint)
import Data.Drasil.Concepts.Computation (algorithm)
import qualified Data.Drasil.Concepts.Physics as CP (collision, damping, joint)

import Drasil.GamePhysics.Assumptions (assumpCT, assumpDI, assumpCAJI)

---------------------
--  LIKELY CHANGES --
---------------------

likelyChangesStmt1, likelyChangesStmt2, likelyChangesStmt3,
  likelyChangesStmt4 :: Sentence

--these statements look like they could be parametrized
likelyChangesStmt1 = (S "internal" +:+ getAcc CM.ode :+:
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  phrase library) `maybeChanged` S "in the future"

likelyChangesStmt2 = chgsStart assumpCT $ phrase library `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+
  plural CP.collision)

likelyChangesStmt3 = chgsStart assumpDI $ phrase library `maybeExpanded` (
  S "to include motion with" +:+ phrase CP.damping)

likelyChangesStmt4 = chgsStart assumpCAJI $ phrase library `maybeExpanded` (
  S "to include" +:+ plural CP.joint `sAnd` plural CM.constraint)

lcVODES, lcEC, lcID, lcIJC :: ConceptInstance

lcVODES = cic "lcVODES" likelyChangesStmt1 "Variable-ODE-Solver" likeChgDom
lcEC = cic "lcEC" likelyChangesStmt2 "Expanded-Collisions" likeChgDom
lcID = cic "lcID" likelyChangesStmt3 "Include-Dampening" likeChgDom
lcIJC = cic "lcIJC" likelyChangesStmt4 "Include-Joints-Constraints" likeChgDom

likelyChgs :: [ConceptInstance]
likelyChgs = [lcVODES, lcEC, lcID, lcIJC]

--------------------------------
--UNLIKELY CHANGES --
--------------------------------

unlikelyChangesStmt1, unlikelyChangesStmt2, unlikelyChangesStmt3, unlikelyChangesStmt4 :: Sentence

unlikelyChangesStmt1 = (S "The goal of the system is to simulate the interactions of rigid bodies" !.)
unlikelyChangesStmt2 = (S "There will always be a source of input data external to the software" !.)
unlikelyChangesStmt3 = (S "A Cartesian Coordinate system is used" !.)
unlikelyChangesStmt4 = (S "All objects are rigid bodies" !.)

ucSRB, ucEI, ucCCS, ucORB :: ConceptInstance

ucSRB = cic "ucSRB" unlikelyChangesStmt1 "Simulate-Rigid-Bodies" unlikeChgDom
ucEI = cic "ucEI" unlikelyChangesStmt2 "External-Input" unlikeChgDom
ucCCS = cic "ucCCS" unlikelyChangesStmt3 "Cartesian-Coordinate-System" unlikeChgDom
ucORB = cic "ucORB" unlikelyChangesStmt4 "Objects-Rigid-Bodies" unlikeChgDom
  
unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [ucSRB, ucEI, ucCCS, ucORB]
