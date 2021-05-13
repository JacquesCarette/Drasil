{-# LANGUAGE PostfixOperators #-}
module Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, gravAccelValue, launchOrigin, pointMass, 
  posXDirection, targetXAxis, timeStartZero, twoDMotion, yAxisGravity) where

import Language.Drasil
import Utils.Drasil

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (assumpDom, value)
import Data.Drasil.Concepts.Math (cartesian, xAxis, xDir, yAxis, yDir)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Concepts.Physics (acceleration, collision, distance, gravity, time, twoD)

import Drasil.Projectile.Concepts (launcher, projectile, target, projMotion)

assumptions :: [ConceptInstance]
assumptions = [twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis, 
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag, pointMass, 
  freeFlight, neglectCurv, timeStartZero, gravAccelValue]

twoDMotion, cartSyst, yAxisGravity, launchOrigin, targetXAxis,
  posXDirection, constAccel, accelXZero, accelYGravity, neglectDrag,
  pointMass, freeFlight, neglectCurv, timeStartZero, 
  gravAccelValue :: ConceptInstance
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
gravAccelValue  = cic "gravAccelValue"  gravAccelValueDesc  "gravAccelValue"  assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = atStartNP (the projMotion) `sIs` phrase twoD +:+. sParen (getAcc twoD)

cartSystDesc :: Sentence
cartSystDesc = S "A" +:+ (phrase cartesian `sIs` S "used") +:+. fromSource neglectCurv

yAxisGravityDesc :: Sentence
yAxisGravityDesc = S "direction" `the_ofThe'` phrase yAxis `sIs` S "directed opposite to" +:+. phrase gravity

launchOriginDesc :: Sentence
launchOriginDesc = (atStartNP (the launcher) `sIs` S "coincident with the origin" !.)

targetXAxisDesc :: Sentence
targetXAxisDesc = atStartNP (the target) +:+ S "lies on the" +:+ phrase xAxis +:+. fromSource neglectCurv

posXDirectionDesc :: Sentence
posXDirectionDesc = S "The positive" +:+ phrase xDir `sIs` S "from the" +:+. (phrase launcher `toThe` phrase target)

constAccelDesc :: Sentence
constAccelDesc = S "The" +:+ (phrase acceleration `sIs` S "constant") +:+.
                 fromSources [accelXZero, accelYGravity, neglectDrag, freeFlight]

accelXZeroDesc :: Sentence
accelXZeroDesc = S "The" +:+ phrase acceleration +:+. (S "in the" +:+ phrase xDir `sIs` S "zero")

accelYGravityDesc :: Sentence
accelYGravityDesc = S "The" +:+ phrase acceleration +:+ S "in the" +:+ phrase yDir `isThe` phrase acceleration +:+
                    S "due to" +:+ phrase gravity +:+. fromSource yAxisGravity

neglectDragDesc :: Sentence
neglectDragDesc = (S "Air drag" `sIs` S "neglected" !.)

pointMassDesc :: Sentence
pointMassDesc = (S "size" `sAnd` S "shape") `the_ofThe'` phrase projectile `sAre`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight" `sIs` S "free; there" `sAre` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `the_ofThe` phrase projectile)

neglectCurvDesc :: Sentence
neglectCurvDesc = S "The" +:+ phrase distance `sIs` S "small enough that" +:+.
                  (S "curvature" `the_ofThe` S "Earth can be neglected")

timeStartZeroDesc :: Sentence
timeStartZeroDesc = atStart time +:+. S "starts at zero"

gravAccelValueDesc :: Sentence
gravAccelValueDesc = S "The" +:+ phrase acceleration +:+ S "due to" +:+
  phrase gravity +:+ S "is assumed to have the" +:+ phrase value +:+ 
  S "provided in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))
