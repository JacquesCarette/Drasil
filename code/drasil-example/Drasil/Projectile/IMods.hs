module Drasil.Projectile.IMods (iMods, landPosIM, messageIM, offsetIM, timeIM) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDerivNoRefs, imNoRefs)
import Utils.Drasil

import qualified Drasil.DocLang.SRS as SRS (valsOfAuxCons)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation, xAxis)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, iSpeed, ixPos,
  ixVel, iyPos, iyVel, time, xConstAccel, xPos, yConstAccel, yPos)

import Drasil.Projectile.Assumptions (accelXZero, accelYGravity, launchOrigin,
  posXDirection, targetXAxis, timeStartZero, yAxisGravity)
import Drasil.Projectile.Concepts (projectile, target)
import Drasil.Projectile.DataDefs (speedIX, speedIY)
import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.GenDefs (posVecGD)
import Drasil.Projectile.Unitals (flightDur, landPos, launAngle, launSpeed,
  message, offset, targPos, tol)

iMods :: [InstanceModel]
iMods = [timeIM, landPosIM, offsetIM, messageIM]

---
timeIM :: InstanceModel
timeIM = imNoRefs timeRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< (sy pi_ / 2)] (qw flightDur) [sy flightDur $> 0]
  (Just timeDeriv) "calOfLandingTime" [angleConstraintNote, gravitationalAccelConstNote, timeConsNote]

timeRC :: RelationConcept
timeRC = makeRC "timeRC" (nounPhraseSP "calculation of landing time")
  EmptyS $ sy flightDur $= 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

timeDeriv :: Derivation
timeDeriv = mkDerivName (phrase flightDur) (weave [timeDerivSents, map E timeDerivEqns])

timeDerivSents :: [Sentence]
timeDerivSents = [timeDerivSent1, timeDerivSent2, timeDerivSent3,
                  timeDerivSent4, timeDerivSent5]

timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4, timeDerivSent5 :: Sentence
timeDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List [eqnWSource (sy iyPos $= 0) launchOrigin,
  eqnWSource (sy yConstAccel $= - sy gravitationalAccelConst) accelYGravity],
  S "Substituting these", plural value, S "into the y-direction" `sOf`
  makeRef2S posVecGD, S "gives us"]
timeDerivSent2 = foldlSentCol [S "To find the", phrase time, S "that the",
  phrase projectile, S "lands" `sC` S "we want to find the", ch time, phrase value,
  sParen (ch flightDur), S "where", E (sy yPos $= 0) +:+. sParen (S "since the" +:+
  phrase target `sIs` S "on the" +:+ phrase xAxis +:+ S "from" +:+ makeRef2S targetXAxis),
  S "From the", phrase equation, S "above",S "we get"]
timeDerivSent3 = foldlSentCol [S "Dividing by", ch flightDur,
  sParen (S "with the" +:+ phrase constraint +:+ E (sy flightDur $> 0)),
  S "gives us"]
timeDerivSent4 = S "Solving for" +:+ ch flightDur +: S "gives us"
timeDerivSent5 = foldlSentCol [S "From", makeRef2S speedIY,
  sParen (S "with" +:+ (E $ sy iSpeed $= sy launSpeed)), S "we can replace", ch iyVel]

timeDerivEqns :: [Expr]
timeDerivEqns = [timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5]

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4, timeDerivEqn5 :: Expr
timeDerivEqn1 = sy yPos $= sy iyVel * sy time - sy gravitationalAccelConst * square (sy time) / 2
timeDerivEqn2 = sy iyVel * sy flightDur - sy gravitationalAccelConst * square (sy flightDur) / 2 $= 0
timeDerivEqn3 = sy iyVel - sy gravitationalAccelConst * sy flightDur / 2 $= 0
timeDerivEqn4 = sy flightDur $= 2 * sy iyVel / sy gravitationalAccelConst
timeDerivEqn5 = sy flightDur $= 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

---
landPosIM :: InstanceModel
landPosIM = imNoRefs landPosRC [qw launSpeed, qw launAngle]
  [sy launSpeed $> 0, 0 $< sy launAngle $< (sy pi_ / 2)] (qw landPos) [sy landPos $> 0]
  (Just landPosDeriv) "calOfLandingDist" [angleConstraintNote, gravitationalAccelConstNote, landPosConsNote]

landPosExpr :: Expr
landPosExpr = sy landPos $= 2 * square (sy launSpeed) * sin (sy launAngle) *
                                cos (sy launAngle) / sy gravitationalAccelConst

landPosRC :: RelationConcept
landPosRC = makeRC "landPosRC" (nounPhraseSP "calculation of landing position")
  landPosConsNote landPosExpr

landPosDeriv :: Derivation
landPosDeriv = mkDerivName (phrase landPos) (weave [landPosDerivSents, map E landPosDerivEqns])

landPosDerivSents :: [Sentence]
landPosDerivSents = [landPosDerivSent1, landPosDerivSent2,
                      landPosDerivSent3, landPosDerivSent4]

landPosDerivSent1, landPosDerivSent2, landPosDerivSent3, landPosDerivSent4 :: Sentence
landPosDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List [eqnWSource (sy ixPos $= 0) launchOrigin,
  eqnWSource (sy xConstAccel $= 0) accelXZero],
  S "Substituting these", plural value, S "into the x-direction" `sOf`
  makeRef2S posVecGD, S "gives us"]
landPosDerivSent2 = foldlSentCol [S "To find the", phrase landPos `sC`
  S "we want to find the", ch xPos, phrase value, sParen (ch landPos),
  S "at", phrase flightDur, sParen (S "from" +:+ makeRef2S timeIM)]
landPosDerivSent3 = foldlSentCol [S "From", makeRef2S speedIX,
  sParen (S "with" +:+ (E $ sy iSpeed $= sy launSpeed)), S "we can replace", ch ixVel]
landPosDerivSent4 = S "Rearranging this gives us the required" +: phrase equation


landPosDerivEqns :: [Expr]
landPosDerivEqns = [landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3, landPosExpr]

landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: Expr
landPosDerivEqn1 = sy xPos $= sy ixVel * sy time
landPosDerivEqn2 = sy landPos $= sy ixVel * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed * cos (sy launAngle) * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs offsetRC [qw landPos, qw targPos]
  [sy landPos $> 0, sy targPos $> 0] (qw offset)
  [] "offsetIM" [landPosNote, landAndTargPosConsNote]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
  EmptyS $ sy offset $= sy landPos - sy targPos

---
messageIM :: InstanceModel
messageIM = imNoDerivNoRefs messageRC [qw offset, qw targPos]
  [sy targPos $> 0, sy offset $> negate (sy landPos)] (qw message)
  [] "messageIM" [offsetNote, targPosConsNote, offsetConsNote, tolNote]

messageRC :: RelationConcept
messageRC = makeRC "messageRC" (nounPhraseSP "output message") 
  EmptyS $ sy message $= completeCase [case1, case2, case3]
  where case1 = (Str "The target was hit.",        abs (sy offset / sy targPos) $< sy tol)
        case2 = (Str "The projectile fell short.", sy offset $< 0)
        case3 = (Str "The projectile went long.",  sy offset $> 0)

--- Notes

angleConstraintNote, gravitationalAccelConstNote, landAndTargPosConsNote, landPosNote,
  landPosConsNote, offsetNote, offsetConsNote, targPosConsNote,
  timeConsNote, tolNote :: Sentence

angleConstraintNote = foldlSent [S "The", phrase constraint,
  E (0 $< sy launAngle $< (sy pi_ / 2)) `sIs` S "from",
  makeRef2S posXDirection `sAnd` makeRef2S yAxisGravity `sC`
  S "and is shown" `sIn` makeRef2S figLaunch]

gravitationalAccelConstNote = ch gravitationalAccelConst `sIs`
  S "defined in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))

landAndTargPosConsNote = S "The" +:+ plural constraint +:+
  E (sy landPos $> 0) `sAnd` E (sy targPos $> 0) `sAre` S "from" +:+. makeRef2S posXDirection

landPosNote = ch landPos `sIs` S "from" +:+. makeRef2S landPosIM

landPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy landPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

offsetNote = ch offset `sIs` S "from" +:+. makeRef2S offsetIM

offsetConsNote = foldlSent [S "The", phrase constraint, E (sy offset $> negate (sy landPos)) `sIs`
  S "from the fact that", E (sy landPos $> 0) `sC` S "from", makeRef2S posXDirection]

targPosConsNote = S "The" +:+ phrase constraint +:+
  E (sy targPos $> 0) `sIs` S "from" +:+. makeRef2S posXDirection

timeConsNote = S "The" +:+ phrase constraint +:+
  E (sy flightDur $> 0) `sIs` S "from" +:+. makeRef2S timeStartZero

tolNote = ch tol `sIs` S "defined in" +:+. makeRef2S (SRS.valsOfAuxCons ([]::[Contents]) ([]::[Section]))
