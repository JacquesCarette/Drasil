module Drasil.Projectile.IMods (distanceIM, iMods, messageIM, offsetIM, timeIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, imNoRefs)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (distance, gravitationalAccel, iSpeed,
  ixPos, ixVel, iyPos, iyVel, time, xConstAccel, xPos, yConstAccel, yPos)

import Drasil.Projectile.Assumptions (accelXZero, accelYGravity, launchOrigin,
  posXDirection, targetXAxis, timeStartZero, yAxisGravity)
import Drasil.Projectile.Concepts (projectile, target)
import Drasil.Projectile.DataDefs (speedIX, speedIY)
import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.GenDefs (posVecGD)
import Drasil.Projectile.Unitals (flightDur, landPos, launAngle, launSpeed,
  message, offset, targPos, tol)

iMods :: [InstanceModel]
iMods = [timeIM, distanceIM, offsetIM, messageIM]

---
timeIM :: InstanceModel
timeIM = imNoRefs timeRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< (sy pi_ / 2)] (qw flightDur)
  [sy flightDur $> 0] timeDeriv "calOfLandingTime" [angleConstraintNote, timeConsNote]

timeRC :: RelationConcept
timeRC = makeRC "timeRC" (nounPhraseSP "calculation of landing time")
  EmptyS $ sy flightDur $= 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

timeDeriv :: Derivation
timeDeriv = (S "Detailed" +: (S "derivation" `sOf` phrase flightDur)) :
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
  phrase projectile, S "lands" `sC` S "we want to find the", ch time,
  phrase value, sParen (ch flightDur), S "where", E (sy yPos $= 0) +:+.
  sParen (S "since the" +:+ phrase target `sIs` S "on the x-axis from" +:+
  makeRef2S targetXAxis), S "From the", phrase equation, S "above", S "we get"]
timeDerivSent3 = foldlSentCol [S "Dividing by", ch flightDur,
  sParen (S "with the" +:+ phrase constraint +:+ E (sy flightDur $> 0)),
  S "gives us"]
timeDerivSent4 = S "Solving for" +:+ ch flightDur +: S "gives us"

timeDerivEqns :: [Expr]
timeDerivEqns = [timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5]

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5 :: Expr
timeDerivEqn1 = sy yPos $= sy iyVel * sy time - sy gravitationalAccel * square (sy time) / 2
timeDerivEqn2 = sy iyVel * sy flightDur - sy gravitationalAccel * square (sy flightDur) / 2 $= 0
timeDerivEqn3 = sy iyVel - sy gravitationalAccel * sy flightDur / 2 $= 0
timeDerivEqn4 = sy flightDur $= 2 * sy iyVel / sy gravitationalAccel
timeDerivEqn5 = sy flightDur $= 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

---
distanceIM :: InstanceModel
distanceIM = imNoRefs distanceRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< (sy pi_ / 2)] (qw landPos)
  [sy landPos $> 0] distanceDeriv "calOfLandingDist" [angleConstraintNote, landPosConsNote]

distanceExpr :: Expr
distanceExpr = sy landPos $= 2 * square (sy iSpeed) * sin (sy launAngle) *
                                cos (sy launAngle) / sy gravitationalAccel

distanceRC :: RelationConcept
distanceRC = makeRC "distanceRC" (nounPhraseSP "calculation of landing distance")
  landPosConsNote distanceExpr

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
  S "travelled" `sC` S "we want to find the", ch xPos, phrase value,
  sParen (ch landPos), S "at", phrase flightDur,
  sParen (S "from" +:+ makeRef2S timeIM)]
distanceDerivSent3 = S "Rearranging this" +:+ phrase equation +: S "gives us"

distanceDerivEqns :: [Expr]
distanceDerivEqns = [distanceDerivEqn1, distanceDerivEqn2, distanceDerivEqn3, distanceExpr]

distanceDerivEqn1, distanceDerivEqn2, distanceDerivEqn3 :: Expr
distanceDerivEqn1 = sy xPos $= sy ixVel * sy time
distanceDerivEqn2 = sy landPos $= sy ixVel * 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel
distanceDerivEqn3 = sy landPos $= sy iSpeed * cos (sy launAngle) * 2 * sy iSpeed * sin (sy launAngle) / sy gravitationalAccel

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs offsetRC [qw landPos, qw targPos]
  [sy landPos $> 0, sy targPos $> 0] (qw offset) [] "offsetIM" [landPosNote, landAndTargPosConsNote]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
  EmptyS $ sy offset $= sy landPos - sy targPos

---
messageIM :: InstanceModel
messageIM = imNoDerivNoRefs messageRC [qw offset, qw targPos]
  [sy targPos $> 0] (qw message) [] "messageIM" [offsetNote, targPosConsNote]

messageRC :: RelationConcept
messageRC = makeRC "messageRC" (nounPhraseSP "output message") 
  EmptyS $ sy message $= case_ [case1, case2, case3]
  where case1 = (Str "The target was hit.",        UnaryOp Abs (sy offset / sy targPos) $< sy tol)
        case2 = (Str "The projectile fell short.", sy offset $< 0)
        case3 = (Str "The projectile went long.",  sy offset $> 0)

--- Notes

angleConstraintNote, landAndTargPosConsNote, landPosNote, landPosConsNote, offsetNote, targPosConsNote, timeConsNote :: Sentence

angleConstraintNote = foldlSent [S "The", phrase constraint,
  E (0 $< sy launAngle $< (sy pi_ / 2)) `sIs` S "from",
  makeRef2S posXDirection `sAnd` makeRef2S yAxisGravity `sC`
  S "and is shown" `sIn` makeRef2S figLaunch]

landAndTargPosConsNote = S "The" +:+ plural constraint +:+
  E (sy landPos $> 0) `sAnd` E (sy targPos $> 0) `sAre` S "from" +:+. makeRef2S posXDirection

landPosNote = ch landPos `sIs` S "from" +:+. makeRef2S distanceIM

landPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy landPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

offsetNote = ch offset `sIs` S "from" +:+. makeRef2S offsetIM

targPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy targPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

timeConsNote = S "The" +:+ phrase constraint +:+
  E (sy flightDur $> 0) `sIs` S "from" +:+. makeRef2S timeStartZero