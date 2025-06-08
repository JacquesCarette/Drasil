module Drasil.GamePhysics.Assumptions where

import Language.Drasil hiding (organization)

import Data.Drasil.Concepts.Documentation as Doc (simulation, assumpDom)
import qualified Data.Drasil.Concepts.Physics as CP (collision, damping, force,
  friction, joint, rigidBody)
import qualified Data.Drasil.Concepts.Math as CM (cartesian, constraint, rightHand)

import Drasil.GamePhysics.Concepts (twoD)

assumptions :: [ConceptInstance]
assumptions = [assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI]

assumpOT, assumpOD, assumpCST, assumpAD, assumpCT, assumpDI,
  assumpCAJI :: ConceptInstance
assumpOT = cic "assumpOT" (foldlSent assumpOTDesc) "objectTy" assumpDom
assumpOD = cic "assumpOD" (foldlSent assumpODDesc) "objectDimension" assumpDom
assumpCST = cic "assumpCST" (foldlSent assumpCSTDesc) "coordinateSystemTy" assumpDom
assumpAD = cic "assumpAD" (foldlSent assumpADDesc) "axesDefined" assumpDom
assumpCT = cic "assumpCT" (foldlSent assumpCTDesc) "collisionType" assumpDom
assumpDI = cic "assumpDI" (foldlSent assumpDIDesc) "dampingInvolvement" assumpDom
assumpCAJI = cic "assumpCAJI" (foldlSent assumpCAJIDesc) "constraintsAndJointsInvolvement" assumpDom

assumpOTDesc, assumpODDesc, assumpCSTDesc, assumpADDesc, assumpCTDesc, 
  assumpDIDesc, assumpCAJIDesc :: [Sentence]

allObject :: Sentence -> [Sentence]
allObject thing = [S "All objects are", thing]

thereNo :: [Sentence] -> [Sentence]
thereNo [x]      = [S "There is no", x, S "involved throughout the", 
  phrase simulation]
thereNo l        = [S "There are no", foldlList Comma List l, S "involved throughout the", 
  phrase simulation]

implies :: Sentence -> [Sentence]
implies f = [S "and this implies that there are no", f] 
--not sure if defining a new function is the best way to do this,
-- as was done in the original file,but it displays correctly
--(line 52 was added for assumption6)

assumpOTDesc = allObject (plural CP.rigidBody)
assumpODDesc = allObject (short twoD)
assumpCSTDesc = [S "The library uses a", phrase CM.cartesian]
assumpADDesc = [S "The axes are defined using", phrase CM.rightHand]
assumpCTDesc = [S "All", plural CP.rigidBody, plural CP.collision,
  S "are vertex-to-edge", plural CP.collision]

assumpDIDesc = thereNo [phrase CP.damping] ++ implies (phrase CP.friction +:+ plural CP.force)
assumpCAJIDesc = thereNo [plural CM.constraint, plural CP.joint]

{-assumptions_list = enumSimple 1 (short assumption) $ map (foldlSent) 
  [assumpOTDesc, assumpODDesc, assumpCSTDesc, assumpADDesc, assumpCTDesc, 
  assumpDIDesc, assumpCAJIDesc]-}

assumptionsListA :: [[Sentence]]
assumptionsListA = [assumpOTDesc, assumpODDesc, assumpCSTDesc, assumpADDesc,
  assumpCTDesc, assumpDIDesc, assumpCAJIDesc]
