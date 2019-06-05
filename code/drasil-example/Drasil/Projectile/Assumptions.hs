module Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, launchOrigin, pointMass, targetXAxis, twoDMotion) where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumpDom)
import Data.Drasil.Concepts.Math (perp)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, cartesian, collision, rightHand, time, twoD)

import Drasil.Projectile.Concepts (launcher, projectile, target)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion, cartSyst, yAxisPerpend, rightHandAxes, launchOrigin,
  targetXAxis, posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  constMass, pointMass, freeFlight, timeStartZero]

twoDMotion, cartSyst, yAxisPerpend, rightHandAxes, launchOrigin, targetXAxis,
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag, constMass,
  pointMass, freeFlight, timeStartZero :: ConceptInstance
twoDMotion      = cic "twoDMotion"      twoDMotionDesc      "twoDMotion"      assumpDom
cartSyst        = cic "cartSyst"        cartSystDesc        "cartSyst"        assumpDom
yAxisPerpend    = cic "yAxisPerpend"    yAxisPerpendDesc    "yAxisPerpend"    assumpDom
rightHandAxes   = cic "rightHandAxes"   rightHandAxesDesc   "rightHandAxes"   assumpDom
launchOrigin    = cic "launchOrigin"    launchOriginDesc    "launchOrigin"    assumpDom
targetXAxis     = cic "targetXAxis"     targetXAxisDesc     "targetXAxis"     assumpDom
posXDirection   = cic "posXDirection"   posXDirectionDesc   "posXDirection"   assumpDom
constAccel      = cic "constAccel"      constAccelDesc      "constAccel"      assumpDom
accelXZero      = cic "accelXZero"      accelXZeroDesc      "accelXZero"      assumpDom
accelYGravity   = cic "accelYGravity"   accelYGravityDesc   "accelYGravity"   assumpDom
neglectDrag     = cic "neglectDrag"     neglectDragDesc     "neglectDrag"     assumpDom
constMass       = cic "constMass"       constMassDesc       "constMass"       assumpDom
pointMass       = cic "pointMass"       pointMassDesc       "pointMass"       assumpDom
freeFlight      = cic "freeFlight"      freeFlightDesc      "freeFlight"      assumpDom
timeStartZero   = cic "timeStartZero"   timeStartZeroDesc   "timeStartZero"   assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = S "The" +:+ phrase projectile +:+ S "motion is in" +:+. getAcc twoD

cartSystDesc :: Sentence
cartSystDesc = S "A" +:+ phrase cartesian +:+. S "is used"

yAxisPerpendDesc :: Sentence
yAxisPerpendDesc = S "The y-axis" `sIs` phrase perp `toThe` S "x-axis."

rightHandAxesDesc :: Sentence
rightHandAxesDesc = S "The axes" `sAre` S "defined using a" +:+. phrase rightHand

launchOriginDesc :: Sentence
launchOriginDesc = S "The" +:+. (phrase launcher `sIs` S "coincident with the origin")  

targetXAxisDesc :: Sentence
targetXAxisDesc = S "The" +:+ phrase target +:+. S "lies on the x-axis"

posXDirectionDesc :: Sentence
posXDirectionDesc = S "The positive x-direction is from the" +:+. (phrase launcher `toThe` phrase target)

constAccelDesc :: Sentence
constAccelDesc = S "The" +:+. (phrase acceleration `sIs` S "constant")

accelXZeroDesc :: Sentence
accelXZeroDesc = S "The" +:+ phrase acceleration +:+. (S "in the x-direction" `sIs` S "zero")

accelYGravityDesc :: Sentence
accelYGravityDesc = S "The" +:+ phrase acceleration +:+ S "in the y-direction" `isThe`
                    phrase acceleration +:+. S "due to gravity"

neglectDragDesc :: Sentence
neglectDragDesc = S "Air drag" `sIs` S "neglected."

constMassDesc :: Sentence
constMassDesc = phrase mass `ofThe'` phrase projectile `sIs` S "constant."

pointMassDesc :: Sentence
pointMassDesc = (S "size" `sAnd` S "shape") `ofThe'` phrase projectile `sAre`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight is free; there" `sAre` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `ofThe` phrase projectile)

timeStartZeroDesc :: Sentence
timeStartZeroDesc = at_start time +:+. S "starts at zero"
