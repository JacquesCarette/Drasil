module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, iSpeed, position)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, second)
import Data.Drasil.Units.Physics (velU)

import Drasil.Projectile.Concepts (landingPos, launcher, launchAngle,
  launchDur, launchSpeed, projectile, targetPos, target)

unitalQuants :: [QuantityDict]
unitalQuants = quantDicts ++ map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = map nw quantDicts ++ map nw constrained

constrained :: [ConstrConcept]
constrained = [landPos, launAngle, launDur, launSpeed, targPos]

quantDicts :: [QuantityDict]
quantDicts = [isShort, offset, isHit]

---
landPos, launAngle, launDur, launSpeed, targPos :: ConstrConcept
landPos   = constrained' (dqd' landPosConcept   (const $ Concat [lP, Atomic "'"])  Real (Just metre))  [gtZeroConstr] (dbl 1)
launAngle = constrained' (dqd' launAngleConcept (const lTheta)                     Real (Just degree)) [gtZeroConstr] (dbl 1)
launDur   = constrained' (dqd' launDurConcept   (const $ Concat [lT, Atomic "'"])  Real (Just second)) [gtZeroConstr] (dbl 1)
launSpeed = constrained' (dqd' launSpeedConcept (const $ sup lV lI)                Real (Just velU))   [gtZeroConstr] (dbl 1)
targPos   = constrained' (dqd' targPosConcept   (const $ sub lP $ Atomic "target") Real (Just metre))  [gtZeroConstr] (dbl 1)

landPosConcept :: ConceptChunk
landPosConcept = cc' landingPos
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher, S "to",
            (S "final" +:+ phrase position) `ofThe` phrase projectile])

launAngleConcept :: ConceptChunk
launAngleConcept = cc' launchAngle
  (foldlSent [S "The", phrase angle, S "between the", phrase launcher `sAnd` S "a straight line",
             S "from the", phrase launcher `toThe` phrase target])

launSpeedConcept :: ConceptChunk
launSpeedConcept = cc' launchSpeed (phrase iSpeed `ofThe'` phrase projectile +:+. S "when launched")

targPosConcept :: ConceptChunk
targPosConcept = cc' targetPos
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher `toThe` phrase target])

launDurConcept :: ConceptChunk
launDurConcept = cc' launchDur (S "The time when the" +:+ phrase projectile +:+. S "lands")

---

isShort :: QuantityDict
isShort = vc "isShort"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase targetPos +:+
   S "is greater than the" +:+ phrase landingPos))
  (Atomic "isShort") Boolean

offset :: QuantityDict
offset = vcUnit "offset"
  (nounPhraseSent (S "offset between the" +:+ phrase targetPos `andThe` phrase landingPos))
  (sub lD $ Atomic "offset") Real metre

isHit :: QuantityDict
isHit = vc "isHit"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase landingPos +:+
   S "is within a degree of tolerance of the" +:+ phrase targetPos))
  (Atomic "isHit") Boolean
