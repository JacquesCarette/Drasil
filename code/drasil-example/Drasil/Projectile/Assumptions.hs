module Drasil.Projectile.Assumptions (accelGravityY, accelZeroX, assumptions, equalHeights) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation as Doc (assumpDom)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, collision, twoD)

import Drasil.Projectile.Concepts (launcher, projectile, target)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion, pointMass, equalHeights, accelZeroX, accelGravityY,
  ignoreCurvature, freeFlight]

twoDMotion, pointMass, equalHeights, accelZeroX, accelGravityY,
  ignoreCurvature, freeFlight :: ConceptInstance
twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
equalHeights    = cic "equalHeights"    equalHeightsDesc    "equalHeights"    assumpDom
accelZeroX      = cic "accelZeroX"      accelZeroXDesc      "accelZeroX"      assumpDom
accelGravityY   = cic "accelGravityY"   accelGravityYDesc   "accelGravityY"   assumpDom
ignoreCurvature = cic "ignoreCurvature" ignoreCurvatureDesc "ignoreCurvature" assumpDom
freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = S "The" +:+ phrase projectile +:+ S "motion is in" +:+. getAcc twoD

pointMassDesc :: Sentence
pointMassDesc = S "The" +:+ phrase projectile +:+ S "is a point" +:+. phrase mass

equalHeightsDesc :: Sentence
equalHeightsDesc = S "The heights of the" +:+ phrase launcher `sAnd` phrase target +:+. S "are equal"

accelZeroXDesc :: Sentence
accelZeroXDesc = at_start acceleration +:+. S "is zero in the x-direction"

accelGravityYDesc :: Sentence
accelGravityYDesc = at_start acceleration +:+. S "in the y-direction is only caused by gravity"

ignoreCurvatureDesc :: Sentence
ignoreCurvatureDesc = S "The effects of the Earth's curvature are ignored."

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight is free; there are no" +:+ plural collision +:+
                 S "during the trajectory of the" +:+. phrase projectile
