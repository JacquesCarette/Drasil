module Drasil.Projectile.GenDefs (genDefns) where

import Prelude hiding (cos, sin)
import Language.Drasil
import Theory.Drasil (GenDefn, gdNoRefs)
import Utils.Drasil

import Data.Drasil.Quantities.Physics (fSpeed, iSpeed, ixVel, scalarAccel, time, xAccel, xDist, yAccel, yVel)

import Drasil.Projectile.Assumptions (accelGravityY, accelZeroX, equalHeights)
import Drasil.Projectile.DataDefs (speedY)
import Drasil.Projectile.Unitals (launAngle)

genDefns :: [GenDefn]
genDefns = [finalSpeedGD, airTimeGD, distanceGD, distanceRefinedGD]

----------
finalSpeedGD :: GenDefn
finalSpeedGD = gdNoRefs finalSpeedRC (getUnit fSpeed) [{-Derivation-}] "finalSpeed" [EmptyS]

finalSpeedRC :: RelationConcept
finalSpeedRC = makeRC "finalSpeedRC" (nounPhraseSP "final speed") EmptyS finalSpeedRel

finalSpeedRel :: Relation
finalSpeedRel = sy fSpeed $= sy iSpeed + sy scalarAccel * sy time

----------
airTimeGD :: GenDefn
airTimeGD = gdNoRefs airTimeRC (getUnit time) [airTimeDeriv] "airTime" [EmptyS]

airTimeRC :: RelationConcept
airTimeRC = makeRC "airTimeR" (nounPhraseSP "air time") EmptyS airTimeRel

airTimeRel :: Relation
airTimeRel = sy time $= BinaryOp Frac (2 * sy iSpeed * sin (sy launAngle)) (sy yAccel)

airTimeDeriv :: Sentence
airTimeDeriv = foldlSent [at_start airTimeGD, S "is derived from" +:+.
  makeRef2S speedY `sAnd` makeRef2S finalSpeedGD, S "It also comes from the",
  S "fact that the", phrase yVel, S "at the maximum height is zero" `sAnd`
  S "that the maximum height is the halfway point of the trajectory",
  sParen (S "from" +:+ makeRef2S equalHeights)]

----------
distanceGD :: GenDefn
distanceGD = gdNoRefs distanceRC (getUnit xDist) [{-Derivation-}] "distance" [makeRef2S accelGravityY]

distanceRC :: RelationConcept
distanceRC = makeRC "distanceRC" (nounPhraseSP "distance in the x-direction") EmptyS distanceRel

distanceRel :: Relation
distanceRel = sy xDist $= sy ixVel * sy time + BinaryOp Frac (sy xAccel * square (sy time)) 2

----------
distanceRefinedGD :: GenDefn
distanceRefinedGD = gdNoRefs distanceRefinedRC (getUnit xDist) [distanceRefinedDeriv] "distanceRefined" [makeRef2S accelZeroX]

distanceRefinedRC :: RelationConcept
distanceRefinedRC = makeRC "distanceRefinedRC" (nounPhraseSP "distance in the x-direction (refined)") EmptyS distanceRefinedRel

distanceRefinedRel :: Relation
distanceRefinedRel = sy xDist $= BinaryOp Frac (2 * square (sy iSpeed) * sin (sy launAngle) *
                      cos (sy launAngle)) (sy yAccel)

distanceRefinedDeriv :: Sentence
distanceRefinedDeriv = foldlSent [at_start distanceRefinedGD, S "is derived from",
  foldlList Comma List $ map makeRef2S [airTimeGD, distanceGD]]
