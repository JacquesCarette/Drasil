module Drasil.GamePhysics.Assumptions where

import Language.Drasil hiding (organization)

import Data.Drasil.Concepts.Documentation as Doc (simulation, assumpDom)
import Data.Drasil.SentenceStructures (FoldType(..), SepType(..),
  foldlList, foldlSent)
import Drasil.GamePhysics.Concepts (twoD)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody,  
  cartesian, rightHand, collision, joint, damping, force, friction)
import qualified Data.Drasil.Concepts.Math as CM (constraint)

newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7]

assumptions :: [ConceptInstance]
assumptions = [assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI]

newA1, newA2, newA3, newA4, newA5, newA6, newA7 :: AssumpChunk
assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI :: ConceptInstance
newA1 = assump "assumpOT" (foldlSent assumptions_assum1) "objectTy"
newA2 = assump "assumpOD" (foldlSent assumptions_assum2) "objectDimension"
newA3 = assump "assumpCST" (foldlSent assumptions_assum3) "coordinateSystemTy"
newA4 = assump "assumpAD" (foldlSent assumptions_assum4) "axesDefined"
newA5 = assump "assumpCT" (foldlSent assumptions_assum5) "collisionType"
newA6 = assump "assumpDI" (foldlSent assumptions_assum6) "dampingInvolvement"
newA7 = assump "assumpCAJI" (foldlSent assumptions_assum7) "constraintsAndJointsInvolvement"

assumpOT = cic "assumpOT" (foldlSent assumptions_assum1) "objectTy" assumpDom
assumpOD = cic "assumpOD" (foldlSent assumptions_assum2) "objectDimension" assumpDom
assumpCST = cic "assumpCST" (foldlSent assumptions_assum3) "coordinateSystemTy" assumpDom
assumpAD = cic "assumpAD" (foldlSent assumptions_assum4) "axesDefined" assumpDom
assumpCT = cic "assumpCT" (foldlSent assumptions_assum5) "collisionType" assumpDom
assumpDI = cic "assumpDI" (foldlSent assumptions_assum6) "dampingInvolvement" assumpDom
assumpCAJI = cic "assumpCAJI" (foldlSent assumptions_assum7) "constraintsAndJointsInvolvement" assumpDom

assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7 :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  (phrase simulation)]
thereNo l        = [S "There are no", foldlList Comma List l, S "involved throughout the", 
  (phrase simulation)]

implies :: Sentence -> [Sentence]
implies f = [S "and this implies that there are no", f] 
--not sure if defining a new function is the best way to do this,
-- as was done in the original file,but it displays correctly
--(line 52 was added for assumption6)

assumptions_assum1 = allObject (plural CP.rigidBody)
assumptions_assum2 = allObject (getAcc twoD)
assumptions_assum3 = [S "The library uses a", (phrase CP.cartesian)]
assumptions_assum4 = [S "The axes are defined using", 
  (phrase CP.rightHand)]
assumptions_assum5 = [S "All", (plural CP.rigidBody), 
  (plural CP.collision), S "are vertex-to-edge", 
  (plural CP.collision)]

assumptions_assum6 = (thereNo [(phrase CP.damping)]) ++ (implies (phrase CP.friction +:+ plural CP.force))
assumptions_assum7 = thereNo [(plural CM.constraint), (plural CP.joint)]

{-assumptions_list = enumSimple 1 (getAcc assumption) $ map (foldlSent) 
  [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7]-}

assumptions_list_a :: [[Sentence]]
assumptions_list_a = [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4,
  assumptions_assum5, assumptions_assum6, assumptions_assum7]
