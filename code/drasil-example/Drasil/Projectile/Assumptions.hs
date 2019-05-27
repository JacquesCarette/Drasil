module Drasil.Projectile.Assumptions (assumptions) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation as Doc (assumpDom)

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
twoDMotionDesc = EmptyS

pointMassDesc :: Sentence
pointMassDesc = EmptyS

equalHeightsDesc :: Sentence
equalHeightsDesc = EmptyS

accelZeroXDesc :: Sentence
accelZeroXDesc = EmptyS

accelGravityYDesc :: Sentence
accelGravityYDesc = EmptyS

ignoreCurvatureDesc :: Sentence
ignoreCurvatureDesc = EmptyS

freeFlightDesc :: Sentence
freeFlightDesc = EmptyS