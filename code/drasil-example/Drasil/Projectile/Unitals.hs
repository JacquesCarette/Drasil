module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, iSpeed, position)

import Data.Drasil.Quantities.Physics (xDist)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, second)
import Data.Drasil.Units.Physics (velU)

import Drasil.Projectile.Concepts (launcher, launchAngle, launchDist,
  launchDur, launchSpeed, projectile, targetDist, target)

unitalQuants :: [QuantityDict]
unitalQuants = quantDicts ++ map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = map nw quantDicts ++ map nw constrained

constrained :: [ConstrConcept]
constrained = [launAngle, launDist, launDur, launSpeed, targDist]

quantDicts :: [QuantityDict]
quantDicts = [isShort, offset, isHit]

---
launAngle, launDist, launDur, launSpeed, targDist :: ConstrConcept
launAngle = constrained' (dqd' launAngleConcept (const lTheta)                     Real (Just degree)) [gtZeroConstr] (dbl 1)
launDist  = constrained' (dqd' launDistConcept  (const $ Concat [lD, Atomic "'"])  Real (Just metre))  [gtZeroConstr] (dbl 1)
launDur   = constrained' (dqd' launDurConcept   (const $ Concat [lT, Atomic "'"])  Real (Just second)) [gtZeroConstr] (dbl 1)
launSpeed = constrained' (dqd' launSpeedConcept (const $ sup lV lI)                Real (Just velU))   [gtZeroConstr] (dbl 1)
targDist  = constrained' (dqd' targDistConcept  (const $ sub lR $ Atomic "target") Real (Just metre))  [gtZeroConstr] (dbl 1)

launDistConcept :: ConceptChunk
launDistConcept = cc' launchDist
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher, S "to",
            (S "final" +:+ phrase position) `ofThe` phrase projectile])

launAngleConcept :: ConceptChunk
launAngleConcept = cc' launchAngle
  (foldlSent [S "The", phrase angle, S "between the", phrase launcher `sAnd` S "a straight line",
             S "from the", phrase launcher `toThe` phrase target])

launSpeedConcept :: ConceptChunk
launSpeedConcept = cc' launchSpeed (phrase iSpeed `ofThe'` phrase projectile +:+. S "when launched")

targDistConcept :: ConceptChunk
targDistConcept = cc' targetDist
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher `toThe` phrase target])

launDurConcept :: ConceptChunk
launDurConcept = cc' launchDur (S "The time when the" +:+ phrase projectile +:+. S "lands")

---

isShort :: QuantityDict
isShort = vc "isShort"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase targetDist +:+
   S "is greater than the" +:+ phrase launchDist))
  (Atomic "isShort") Boolean

offset :: QuantityDict
offset = vcUnit "offset"
  (nounPhraseSent (S "offset between the" +:+ phrase targetDist `andThe` phrase launchDist))
  (sub lR $ Atomic "offset") Real metre

isHit :: QuantityDict
isHit = vc "isHit"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase launchDist +:+
   S "is within a degree of tolerance of the" +:+ phrase targetDist))
  (Atomic "isHit") Boolean
