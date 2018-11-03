module Drasil.GamePhysics.Assumptions where

import Control.Lens ((^.))

import Language.Drasil hiding (organization)

import Data.Drasil.Concepts.Documentation as Doc (simulation, assumpDom)
import Data.Drasil.SentenceStructures (FoldType(..), SepType(..),
  foldlList, foldlSent)
import Drasil.GamePhysics.Concepts (twoD)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody,  
  cartesian, rightHand, collision, joint, damping)
import qualified Data.Drasil.Concepts.Math as CM (constraint)


newAssumptions :: [AssumpChunk]
newAssumptions = [newA1, newA2, newA3, newA4, newA5, newA6, newA7]

assumptions :: [ConceptInstance]
assumptions = [assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI]

newA1, newA2, newA3, newA4, newA5, newA6, newA7 :: AssumpChunk
assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI :: ConceptInstance
newA1 = assump "objectTy" (foldlSent assumptions_assum1) (mkLabelRAAssump' "objectTy")
assumpOT = cic "assumpOT" (foldlSent assumptions_assum1) "objectTy" assumpDom
newA2 = assump "objectDimension" (foldlSent assumptions_assum2) (mkLabelRAAssump' "objectDimension")
assumpOD = cic "assumpOD" (foldlSent assumptions_assum2) "objectDimension" assumpDom
newA3 = assump "coordinateSystemTy" (foldlSent assumptions_assum3) (mkLabelRAAssump' "coordinateSystemTy")
assumpCST = cic "assumpCST" (foldlSent assumptions_assum3) "coordinateSystemTy" assumpDom
newA4 = assump "axesDefined" (foldlSent assumptions_assum4) (mkLabelRAAssump' "axesDefined")
assumpAD = cic "assumpAD" (foldlSent assumptions_assum4) "axesDefined" assumpDom
newA5 = assump "collisionType" (foldlSent assumptions_assum5) (mkLabelRAAssump' "collisionType")
assumpCT = cic "assumpCT" (foldlSent assumptions_assum5) "collisionType" assumpDom
newA6 = assump "dampingInvolvement" (foldlSent assumptions_assum6) (mkLabelRAAssump' "dampingInvolvement")
assumpDI = cic "assumpDI" (foldlSent assumptions_assum6) "dampingInvolvement" assumpDom
newA7 = assump "constraintsAndJointsInvolvement" (foldlSent assumptions_assum7) (mkLabelRAAssump' "constraintsAndJointsInvolvement")
assumpCAJI = cic "assumpCAJI" (foldlSent assumptions_assum7) "constraintsAndJointsInvolvement" assumpDom

assumptions_list :: [Contents]
assumptions_list = map (LlC . (\x -> mkRawLC (Assumption x) (x ^. getLabel))) newAssumptions

assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7 :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  (phrase simulation)]
thereNo l        = [S "There are no", foldlList Comma List l, S "involved throughout the", 
  (phrase simulation)]

assumptions_assum1 = allObject (plural CP.rigidBody)
assumptions_assum2 = allObject (getAcc twoD)
assumptions_assum3 = [S "The library uses a", (phrase CP.cartesian)]
assumptions_assum4 = [S "The axes are defined using", 
  (phrase CP.rightHand)]
assumptions_assum5 = [S "All", (plural CP.rigidBody), 
  (plural CP.collision), S "are vertex-to-edge", 
  (plural CP.collision)]

assumptions_assum6 = thereNo [(phrase CP.damping)]
assumptions_assum7 = thereNo [(plural CM.constraint), (plural CP.joint)]

{-assumptions_list = enumSimple 1 (getAcc assumption) $ map (foldlSent) 
  [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4, assumptions_assum5, 
  assumptions_assum6, assumptions_assum7]-}

assumptions_list_a :: [[Sentence]]
assumptions_list_a = [assumptions_assum1, assumptions_assum2, assumptions_assum3, assumptions_assum4,
  assumptions_assum5, assumptions_assum6, assumptions_assum7]