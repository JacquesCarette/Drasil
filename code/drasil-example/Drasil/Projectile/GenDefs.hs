module Drasil.Projectile.GenDefs (accelGravityY, genDefns) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)

import Data.Drasil.Quantities.Physics (acceleration, distance, time, velocity)

import Data.Drasil.SentenceStructures (FoldType(List), SepType(Comma), foldlList, foldlSent, sAnd)

import Drasil.Projectile.Assumptions (accelGravityY, accelZeroX, equalHeights)
import Drasil.Projectile.DataDefs (velY)
import Drasil.Projectile.Unitals (projAngle, vi, vf)

genDefns :: [GenDefn]
genDefns = [finalVelocityGD, airTimeGD, distanceGD, distanceRefinedGD]

----------
finalVelocityGD :: GenDefn
finalVelocityGD = gdNoRefs finalVelocityRC (getUnit vf) [{-Derivation-}] "finalVelocity" [EmptyS]

finalVelocityRC :: RelationConcept
finalVelocityRC = makeRC "finalVelocityRC" (nounPhraseSP "final velocity") EmptyS finalVelocityRel

finalVelocityRel :: Relation
finalVelocityRel = (sy vf) $= (sy vi) + (sy acceleration) * (sy time)

----------
airTimeGD :: GenDefn
airTimeGD = gdNoRefs airTimeRC (getUnit time) [airTimeDeriv] "airTime" [EmptyS]

airTimeRC :: RelationConcept
airTimeRC = makeRC "airTimeR" (nounPhraseSP "air time") EmptyS airTimeRel

airTimeRel :: Relation
airTimeRel = (sy time) $= BinaryOp Frac (2 * sy vi * sin (sy projAngle)) (UnaryOp Abs (sy acceleration))

airTimeDeriv :: Sentence
airTimeDeriv = foldlSent [at_start airTimeGD, S "is derived from" +:+.
  makeRef2S velY `sAnd` makeRef2S finalVelocityGD, S "It also comes from the",
  S "fact that the", phrase velocity, S "at the maximum height is zero" `sAnd`
  S "that the maximum height is the halfway point of the trajectory",
  sParen (S "from" +:+ makeRef2S equalHeights)]

----------
distanceGD :: GenDefn
distanceGD = gdNoRefs distanceRC (getUnit distance) [{-Derivation-}] "distance" [makeRef2S accelGravityY]

distanceRC :: RelationConcept
distanceRC = makeRC "distanceRC" (nounPhraseSP "distance") EmptyS distanceRel

distanceRel :: Relation
distanceRel = (sy distance) $= (sy vi) * (sy time) + BinaryOp Frac ((sy acceleration) * square (sy time)) 2

----------
distanceRefinedGD :: GenDefn
distanceRefinedGD = gdNoRefs distanceRefinedRC (getUnit distance) [distanceRefinedDeriv] "distanceRefined" [makeRef2S accelZeroX]

distanceRefinedRC :: RelationConcept
distanceRefinedRC = makeRC "distanceRefinedRC" (nounPhraseSP "distance (refined)") EmptyS distanceRefinedRel

distanceRefinedRel :: Relation
distanceRefinedRel = (sy distance) $= BinaryOp Frac (2 * square (sy vi) * sin (sy projAngle) *
                      cos (sy projAngle)) (UnaryOp Abs (sy acceleration))

distanceRefinedDeriv :: Sentence
distanceRefinedDeriv = foldlSent [at_start distanceRefinedGD, S "is derived from",
  foldlList Comma List $ map makeRef2S [airTimeGD, distanceGD]]
