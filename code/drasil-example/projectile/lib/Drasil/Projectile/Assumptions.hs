{-# LANGUAGE PostfixOperators #-}
module Drasil.Projectile.Assumptions (accelYGravity, accelXZero, cartSyst,
  assumptions, constAccel, gravAccelValue, launchOrigin, pointMass,
  posXDirection, targetXAxis, timeStartZero, twoDMotion, yAxisGravity) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.NounPhrase.Combinators as NP
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)
import Drasil.Sentence.Combinators (fromSources)

import Data.Drasil.Concepts.Documentation (assumpDom, value, consVals)
import Data.Drasil.Concepts.Math (cartesian, xAxis, xDir, yAxis, yDir, direction, positive)
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
twoDMotionDesc = D.toSent (atStartNP (NP.the (projMotion `is` twoD))) +:+. sParen (short twoD)

cartSystDesc :: Sentence
cartSystDesc = D.toSent (atStartNP (a_ cartesian)) `S.is` S "used" +:+. fromSource neglectCurv

yAxisGravityDesc :: Sentence
yAxisGravityDesc = D.toSent (atStartNP (direction `the_ofThe` yAxis)) `S.is` S "directed opposite to" +:+. phrase gravity

launchOriginDesc :: Sentence
launchOriginDesc = (D.toSent (atStartNP (the launcher)) `S.is` S "coincident with the origin" !.)

targetXAxisDesc :: Sentence
targetXAxisDesc = D.toSent (atStartNP (the target)) +:+ S "lies on the" +:+ phrase xAxis +:+. fromSource neglectCurv

posXDirectionDesc :: Sentence
posXDirectionDesc = D.toSent (atStartNP (NP.the (combineNINI positive xDir))) `S.is` S "from the" +:+. D.toSent (phraseNP (launcher `toThe` target))

constAccelDesc :: Sentence
constAccelDesc = D.toSent (atStartNP (the acceleration)) `S.is` S "constant" +:+.
                 fromSources [accelXZero, accelYGravity, neglectDrag, freeFlight]

accelXZeroDesc :: Sentence
accelXZeroDesc = D.toSent (atStartNP (NP.the (acceleration `inThe` xDir))) `S.is` (S "zero" !.)

accelYGravityDesc :: Sentence
accelYGravityDesc = D.toSent (atStartNP (NP.the (acceleration `inThe` yDir))) `S.isThe` phrase acceleration +:+
                    S "due to" +:+ phrase gravity +:+. fromSource yAxisGravity

neglectDragDesc :: Sentence
neglectDragDesc = (S "Air drag" `S.is` S "neglected" !.)

pointMassDesc :: Sentence
pointMassDesc = (S "size" `S.and_` S "shape") `S.the_ofTheC` phrase projectile `S.are`
                S "negligible" `sC` S "so that it can be modelled as a point" +:+. phrase mass

freeFlightDesc :: Sentence
freeFlightDesc = S "The flight" `S.is` S "free; there" `S.are` S "no" +:+ plural collision +:+
                 S "during" +:+. (S "trajectory" `S.the_ofThe` phrase projectile)

neglectCurvDesc :: Sentence
neglectCurvDesc = D.toSent (atStartNP (the distance)) `S.is` S "small enough that" +:+.
                  (S "curvature" `S.the_ofThe` S "celestial body can be neglected")

timeStartZeroDesc :: Sentence
timeStartZeroDesc = atStart time +:+. S "starts at zero"

gravAccelValueDesc :: Sentence
gravAccelValueDesc = D.toSent (atStartNP (the acceleration)) +:+ S "due to" +:+
  phrase gravity +:+ S "is assumed to have the" +:+ phrase value +:+
  S "provided in the section for" +:+. namedRef (SRS.valsOfAuxCons [] []) (titleize consVals)
