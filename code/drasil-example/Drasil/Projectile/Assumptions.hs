module Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, launchOrigin, pointMass, posXDirection, targetXAxis,
  timeStartZero, twoDMotion) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumpDom)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, cartesian, collision,
  distance, time, twoD)

import Drasil.Projectile.Concepts (launcher, projectile, target)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion, cartSyst, yAxisGravity, launchOrigin,
  targetXAxis, posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  pointMass, freeFlight, neglectCurv, timeStartZero]

twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis,
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  pointMass, freeFlight, neglectCurv, timeStartZero :: ConceptInstance
twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
cartSyst        = cic "cartSyst"        cartSystDesc        "cartSyst"        assumpDom
yAxisGravity    = cic "yAxisGravity"    yAxisGravityDesc    "yAxisGravity"    assumpDom
launchOrigin    = cic "launchOrigin"    launchOriginDesc    "launchOrigin"    assumpDom
targetXAxis     = cic "targetXAxis"     targetXAxisDesc     "targetXAxis"     assumpDom
posXDirection   = cic "posXDirection"   posXDirectionDesc   "posXDirection"   assumpDom
constAccel      = cic "constAccel"      constAccelDesc      "constAccel"      assumpDom
accelXZero      = cic "accelXZero"      accelXZeroDesc      "accelXZero"      assumpDom
accelYGravity   = cic "accelYGravity"   accelYGravityDesc   "accelYGravity"   assumpDom
neglectDrag     = cic "neglectDrag"     neglectDragDesc     "neglectDrag"     assumpDom
pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom
neglectCurv     = cic "neglectCurv"     neglectCurvDesc     "neglectCurv"     assumpDom
timeStartZero   = cic "timeStartZero"   timeStartZeroDesc   "timeStartZero"   assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = S "The" +:+ phrase projectile +:+ S "motion" `sIs` phrase twoD +:+. sParen (getAcc twoD)

cartSystDesc :: Sentence
cartSystDesc = S "A" +:+ (phrase cartesian `sIs` S "used") +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

yAxisGravityDesc :: Sentence
yAxisGravityDesc = S "direction" `ofThe'` S "y-axis is directed opposite to gravity."

launchOriginDesc :: Sentence
launchOriginDesc = S "The" +:+. (phrase launcher `sIs` S "coincident with the origin")  

targetXAxisDesc :: Sentence
targetXAxisDesc = S "The" +:+ phrase target +:+ S "lies on the x-axis" +:+. sParen (S "from" +:+ makeRef2S neglectCurv)

posXDirectionDesc :: Sentence
posXDirectionDesc = S "The positive x-direction" `sIs` S "from the" +:+. (phrase launcher `toThe` phrase target)

constAccelDesc :: Sentence
constAccelDesc = S "The" +:+ (phrase acceleration `sIs` S "constant") +:+.
                 sParen (S "from" +:+ foldlList Comma List (map makeRef2S [accelXZero, accelYGravity, neglectDrag, freeFlight]))

accelXZeroDesc :: Sentence
accelXZeroDesc = S "The" +:+ phrase acceleration +:+. (S "in the x-direction" `sIs` S "zero")

accelYGravityDesc :: Sentence
accelYGravityDesc = S "The" +:+ phrase acceleration +:+ S "in the y-direction" `isThe`
                    phrase acceleration +:+ S "due to gravity" +:+. sParen (S "from" +:+ makeRef2S yAxisGravity)

neglectDragDesc :: Sentence
neglectDragDesc = S "Air drag" `sIs` S "neglected."

pointMassDesc :: Sentence
pointMassDesc = (S "size" `sAnd` S "shape") `ofThe'` phrase projectile `sAre`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight" `sIs` S "free; there" `sAre` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `ofThe` phrase projectile)

neglectCurvDesc :: Sentence
neglectCurvDesc = S "The" +:+ phrase distance `sIs` S "small enough that" +:+.
                  (S "curvature" `ofThe` S "Earth can be neglected")

timeStartZeroDesc :: Sentence
timeStartZeroDesc = atStart time +:+. S "starts at zero"
