module Drasil.Projectile.IMods (iMods) where

import Prelude hiding (cos, sin)

import Language.Drasil
import Theory.Drasil (DataDefinition, InstanceModel, imNoDerivNoRefs, imNoRefs)
import Utils.Drasil
import Data.Drasil.Utils (weave)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Math (constraint, equation)
import Data.Drasil.Quantities.Physics (gravitationalAccel, iSpeed, iyPos, iyVel,
  time, yConstAccel, yPos)

import Drasil.Projectile.Assumptions (accelYGravity, launchOrigin, targetXAxis)
import Drasil.Projectile.Concepts (projectile, target)
import Drasil.Projectile.DataDefs (speedIY)
import Drasil.Projectile.GenDefs (posVecGD)
import Drasil.Projectile.Unitals (isHit, isShort, launAngle, launDist,
  launDur, launSpeed, offset, targDist)

iMods :: [InstanceModel]
iMods = [timeIM, shortIM, offsetIM, hitIM]

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
                  timeDerivSent4, replaceFromDDSent speedIY iyVel]

timeDerivSent1, timeDerivSent2, timeDerivSent3, timeDerivSent4 :: Sentence
timeDerivSent1 = foldlSentCol [S "We know that" +:+.
  foldlList Comma List [eqnFromSource (sy iyPos $= 0) launchOrigin,
  eqnFromSource (sy yConstAccel $= - sy gravitationalAccel) accelYGravity],
  S "Substituting these", plural value, S "into the y-direction" `sOf`
  makeRef2S posVecGD, S "gives us"]
  where
    eqnFromSource a b = E a +:+ sParen (makeRef2S b)
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
shortIM :: InstanceModel
shortIM = imNoDerivNoRefs shortRC [qw targDist, qw launDist]
  [sy targDist $> 0, sy launDist $> 0] (qw isShort) []
  "shortIM" [shortDesc]

shortRC :: RelationConcept
shortRC = makeRC "shortRC" (nounPhraseSP "isShort") 
  shortDesc $ sy isShort $= sy targDist $> sy launDist
  
shortDesc :: Sentence
shortDesc = EmptyS

---
offsetIM :: InstanceModel
offsetIM = imNoDerivNoRefs offsetRC [qw targDist, qw launDist]
  [sy targDist $> 0, sy launDist $> 0] (qw offset) []
  "offsetIM" [offsetDesc]

offsetRC :: RelationConcept
offsetRC = makeRC "offsetRC" (nounPhraseSP "offset") 
  offsetDesc $ sy offset $= UnaryOp Abs (sy targDist - sy launDist)
  
offsetDesc :: Sentence
offsetDesc = EmptyS

---
hitIM :: InstanceModel
hitIM = imNoDerivNoRefs hitRC [qw offset, qw targDist]
  [sy offset $> 0, sy targDist $> 0] (qw isHit) []
  "hitIM" [hitDesc]

hitRC :: RelationConcept
hitRC = makeRC "hitRC" (nounPhraseSP "isHit") 
  hitDesc $ sy isHit $= sy offset $< (0.02 * sy targDist)
  
hitDesc :: Sentence
hitDesc = EmptyS

-- Helpers
-- FIXME: Pull to drasil-utils in Misc
replaceFromDDSent :: DataDefinition -> UnitalChunk -> Sentence
replaceFromDDSent ddef c = S "From" +:+ makeRef2S ddef +:+ S "we can replace" +: E (sy c)
