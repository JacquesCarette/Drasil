module Drasil.Projectile.IMods (iMods) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, imNoRefs)
import Utils.Drasil
import Data.Drasil.Utils (eqnWSource, fromReplace, weave)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation)
import Data.Drasil.Quantities.Physics (distance, gravitationalAccel, iSpeed,
  ixPos, ixVel, iyPos, iyVel, time, xConstAccel, xPos, yConstAccel, yPos)

import Drasil.Projectile.Assumptions (accelXZero, accelYGravity, launchOrigin, targetXAxis)
import Drasil.Projectile.Concepts (projectile, target)
import Drasil.Projectile.DataDefs (speedIX, speedIY)
import Drasil.Projectile.GenDefs (posVecGD)
import Drasil.Projectile.Unitals (isHit, isShort, launAngle, landPos,
  launDur, launSpeed, offset, targPos)

iMods :: [InstanceModel]
iMods = [timeIM, distanceIM, shortIM, offsetIM, hitIM]

---
timeIM :: InstanceModel
timeIM = imNoRefs timeRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< 90] (qw launDur)
  [sy launDur $> 0] timeDeriv "calOfLandingTime" [timeDesc]

timeRC :: RelationConcept
timeRC = makeRC "timeRC" (nounPhraseSP "calculation of landing time")
  timeDesc $ sy launDur $= 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

timeDesc :: Sentence
timeDesc = EmptyS

timeDeriv :: Derivation
timeDeriv = (S "Detailed" +: (S "derivation" `sOf` phrase launDur)) :
               weave [timeDerivSents, map E timeDerivEqns]

timeDerivSents :: [Sentence]
timeDerivSents = [timeDerivSent1, timeDerivSent2, timeDerivSent3,
                  timeDerivSent4, fromReplace speedIY iyVel]

timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4 :: Sentence
timeDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List [eqnWSource (sy iyPos $= 0) launchOrigin,
  eqnWSource (sy yConstAccel $= - sy gravitationalAccel) accelYGravity],
  S "Substituting these", plural value, S "into the y-direction" `sOf`
  makeRef2S posVecGD, S "gives us"]
timeDerivSent2 = foldlSentCol [S "To find the", phrase time, S "that the",
  phrase projectile, S "lands" `sC` S "we want to find the", E (sy time), 
  phrase value, sParen (E (sy launDur)), S "where", E (sy yPos $= 0) +:+.
  sParen (S "since the" +:+ phrase target `sIs` S "on the x-axis from" +:+
  makeRef2S targetXAxis), S "From the", phrase equation, S "above", S "we get"]
timeDerivSent3 = foldlSentCol [S "Divide by", E (sy launDur),
  sParen (S "with the" +:+ phrase constraint +:+ E (sy launDur $> 0)),
  S "to get"]
timeDerivSent4 = S "Solving for" +:+ E (sy launDur) +: S "gives us"

timeDerivEqns :: [Expr]
timeDerivEqns = [timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5]

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5 :: Expr
timeDerivEqn1 = sy yPos $= sy iyVel * sy time - sy gravitationalAccel * square (sy time) / 2
timeDerivEqn2 = sy iyVel * sy launDur - sy gravitationalAccel * square (sy launDur) / 2 $= 0
timeDerivEqn3 = sy iyVel - sy gravitationalAccel * sy launDur / 2 $= 0
timeDerivEqn4 = sy launDur $= 2 * sy iyVel / sy gravitationalAccel
timeDerivEqn5 = sy launDur $= 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

---
distanceIM :: InstanceModel
distanceIM = imNoRefs distanceRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< 90] (qw landPos)
  [sy landPos $> 0] distanceDeriv "calOfLandingDist" [distanceDesc]

distanceExpr :: Expr
distanceExpr = sy landPos $= 2 * square (sy iSpeed) * sin (sy launAngle) *
                                cos (sy launAngle) / sy gravitationalAccel

distanceRC :: RelationConcept
distanceRC = makeRC "distanceRC" (nounPhraseSP "calculation of landing distance") distanceDesc distanceExpr

distanceDesc :: Sentence
distanceDesc = EmptyS

distanceDeriv :: Derivation
distanceDeriv = (S "Detailed" +: (S "derivation" `sOf` phrase landPos)) :
               weave [distanceDerivSents, map E distanceDerivEqns]

distanceDerivSents :: [Sentence]
distanceDerivSents = [distanceDerivSent1, distanceDerivSent2,
                      fromReplace speedIX ixVel, distanceDerivSent3]

distanceDerivSent1, distanceDerivSent2, distanceDerivSent3 :: Sentence
distanceDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List [eqnWSource (sy ixPos $= 0) launchOrigin,
  eqnWSource (sy xConstAccel $= 0) accelXZero],
  S "Substituting these", plural value, S "into the x-direction" `sOf`
  makeRef2S posVecGD, S "gives us"]
distanceDerivSent2 = foldlSentCol [S "To find the horizontal", phrase distance,
  S "travelled" `sC` S "we want to find the", E (sy xPos), phrase value,
  sParen (E (sy landPos)), S "at", phrase launDur,
  sParen (S "from" +:+ makeRef2S timeIM)]
distanceDerivSent3 = S "Rearranging this" +:+ phrase equation +: S "gives us"

distanceDerivEqns :: [Expr]
distanceDerivEqns = [distanceDerivEqn1, distanceDerivEqn2, distanceDerivEqn3, distanceExpr]

distanceDerivEqn1, distanceDerivEqn2, distanceDerivEqn3 :: Expr
distanceDerivEqn1 = sy xPos $= sy ixVel * sy time
distanceDerivEqn2 = sy landPos $= sy ixVel * 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel
distanceDerivEqn3 = sy landPos $= sy iSpeed * cos (sy launAngle) * 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

---
shortIM :: InstanceModel
shortIM = imNoDerivNoRefs shortRC [qw targPos, qw landPos]
  [sy targPos $> 0, sy landPos $> 0] (qw isShort) []
  "shortIM" [shortDesc]

shortRC :: RelationConcept
shortRC = makeRC "shortRC" (nounPhraseSP "isShort") 
  shortDesc $ sy isShort $= sy targPos $> sy landPos
  
shortDesc :: Sentence
shortDesc = EmptyS

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs offsetRC [qw targPos, qw landPos]
  [sy targPos $> 0, sy landPos $> 0] (qw offset) []
  "offsetIM" [offsetDesc]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
  offsetDesc $ sy offset $= UnaryOp Abs (sy targPos - sy landPos)
  
offsetDesc :: Sentence
offsetDesc = EmptyS

---
hitIM :: InstanceModel
hitIM = imNoDerivNoRefs hitRC [qw offset, qw targPos]
  [sy offset $> 0, sy targPos $> 0] (qw isHit) []
  "hitIM" [hitDesc]

hitRC :: RelationConcept
hitRC = makeRC "hitRC" (nounPhraseSP "isHit") 
  hitDesc $ sy isHit $= sy offset $< (0.02 * sy targPos)
  
hitDesc :: Sentence
hitDesc = EmptyS
