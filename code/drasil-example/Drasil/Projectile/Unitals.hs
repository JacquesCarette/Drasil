module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.Concepts.Math (angle)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree)
import Data.Drasil.Units.Physics (velU)

unitalQuants :: [QuantityDict]
unitalQuants = qw projAngle : map qw [vf, vi, vx, vy]

unitalIdeas :: [IdeaDict]
unitalIdeas = nw projAngle : map nw [vf, vi, vx, vy]

projAngle :: ConstrConcept
projAngle = constrained' (dqd' projAngleConcept (const lTheta) Real (Just degree))
  [gtZeroConstr] (dbl 1)

projAngleConcept :: ConceptChunk
projAngleConcept = dccWDS "angle of projectile" (cn "angle of projectile")
  (S "The" +:+ phrase angle +:+ S "between the launcher and a straight line" +:+.
   S "from the launcher to the target")

vf, vi, vx, vy :: UnitalChunk
vf = uc' "vf" (cn "final velocity")          "" (sub lV lF) velU
vi = uc' "vi" (cn "initial velocity")        "" (sub lV lI) velU
vx = uc' "vx" (cn "x-component of velocity") "" (sub lV lX) velU
vy = uc' "vy" (cn "y-component of velocity") "" (sub lV lY) velU
