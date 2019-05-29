module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Utils.Drasil

import Data.Drasil.SentenceStructures (foldlSent)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, position)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre)
import Data.Drasil.Units.Physics (velU)

import Drasil.Projectile.Concepts (launcher, projectile, targetDist, target)

unitalQuants :: [QuantityDict]
unitalQuants = quantDicts ++ map qw constrained ++ map qw [vf, vi, vx, vy]

unitalIdeas :: [IdeaDict]
unitalIdeas = map nw quantDicts ++ map nw constrained ++ map nw [vf, vi, vx, vy]

constrained :: [ConstrConcept]
constrained = [projAngle, projDist, projSpeed, targDist]

quantDicts :: [QuantityDict]
quantDicts = [isShort, offset, isHit]

---
projAngle, projDist, projSpeed, targDist :: ConstrConcept
projAngle = constrained' (dqd' projAngleConcept (const lTheta)                     Real (Just degree)) [gtZeroConstr] (dbl 1)
projDist  = constrained' (dqd' progDistConcept  (const lD)                         Real (Just metre))  [gtZeroConstr] (dbl 1)
projSpeed = constrained' (dqd' projSpeedConcept (const lV)                         Real (Just velU))   [gtZeroConstr] (dbl 1)
targDist  = constrained' (dqd' targDistConcept  (const $ sub lD $ Atomic "target") Real (Just metre))  [gtZeroConstr] (dbl 1)

progDistConcept :: ConceptChunk
progDistConcept = dccWDS "distance of projectile" (cn "distance of projectile")
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher, S "to",
            (S "final" +:+ phrase position) `ofThe` phrase projectile])

projAngleConcept :: ConceptChunk
projAngleConcept = dccWDS "angle of projectile" (cn "angle of projectile")
  (foldlSent [S "The", phrase angle, S "between the", phrase launcher `sAnd` S "a straight line",
             S "from the", phrase launcher, S "to the", phrase target])

projSpeedConcept :: ConceptChunk
projSpeedConcept = dccWDS "speed of projectile" (cn "speed of projectile")
  (S "The initial speed of the" +:+ phrase projectile +:+. S "when launched")

targDistConcept :: ConceptChunk
targDistConcept = cc' targetDist
  (foldlSent [S "The", phrase distance, S "from the", phrase launcher, S "to the", phrase target])

vf, vi, vx, vy :: UnitalChunk
vf = uc' "vf" (cn "final velocity")          "" (sub lV lF) velU
vi = uc' "vi" (cn "initial velocity")        "" (sub lV lI) velU
vx = uc' "vx" (cn "x-component of velocity") "" (sub lV lX) velU
vy = uc' "vy" (cn "y-component of velocity") "" (sub lV lY) velU

---

isShort :: QuantityDict
isShort = vc "isShort" (nounPhraseSP $ "variable that is assigned true when the target distance"
  ++ " is greater than the projectile distance")
  (Atomic "isShort") Boolean

offset :: QuantityDict
offset = vc "offset" (nounPhraseSP $ "the offset between the target distance and the"
  ++ " distance of the projectile")
  (sub lD $ Atomic "offset") Real

isHit :: QuantityDict
isHit = vc "isHit" (nounPhraseSP $ "variable that is assigned true when the projectile distance"
  ++ " is within a degree of tolerance of the target distance")
  (Atomic "isHit") Real
