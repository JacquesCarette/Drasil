module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Data.Drasil.IdeaDicts
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  requirement, srs, typUnc)
import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, iSpeed, position, time, twoD)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (degree, metre, second)
import Data.Drasil.Units.Physics (velU)

import Drasil.Projectile.Concepts (landingPos, launcher, launchAngle,
  launchDur, launchSpeed, projectile, targetPos, target)
import qualified Drasil.Projectile.Concepts as C (offset)

-- FIXME: Move to Defs?
acronyms :: [CI]
acronyms = [twoD, assumption, dataDefn, genDefn, goalStmt, inModel, physSyst, requirement, srs, thModel, typUnc]

inputs :: [QuantityDict]
inputs = map qw [launAngle, launSpeed, targPos]

unitalQuants :: [QuantityDict]
unitalQuants = quantDicts ++ map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = map nw quantDicts ++ map nw constrained

inConstraints :: [UncertQ]
inConstraints = [launAngleUnc, launSpeedUnc, targPosUnc]

outConstraints :: [UncertQ]
outConstraints = [landPosUnc, offsetUnc]

constrained :: [ConstrConcept]
constrained = [landPos, launAngle, launDur, launSpeed, offset, targPos]

quantDicts :: [QuantityDict]
quantDicts = [isShort, isHit]

---
landPosUnc, launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc :: UncertQ
landPosUnc   = uq landPos   defaultUncrt
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset    defaultUncrt
targPosUnc   = uq targPos   defaultUncrt

landPos, launAngle, launDur, launSpeed, offset, targPos :: ConstrConcept
landPos   = constrained' (dqd' landPosConcept   (const $ Concat [lP, Atomic "'"])  Real (Just metre))  [gtZeroConstr] (dbl 1)
launAngle = constrained' (dqd' launAngleConcept (const lTheta)                     Real (Just degree)) [physc $ Bounded (Exc, 0) (Exc, 90)] (int 45)
launDur   = constrained' (dqd' launDurConcept   (const $ Concat [lT, Atomic "'"])  Real (Just second)) [gtZeroConstr] (dbl 1)
launSpeed = constrained' (dqd' launSpeedConcept (const $ sup lV lI)                Real (Just velU))   [gtZeroConstr] (int 100)
offset    = constrained' (dqd' offsetConcept    (const $ sub lD $ Atomic "offset") Real (Just metre))  [gtZeroConstr] (dbl 1)
targPos   = constrained' (dqd' targPosConcept   (const $ sub lP $ Atomic "target") Real (Just metre))  [gtZeroConstr] (int 100)

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

offsetConcept :: ConceptChunk
offsetConcept = cc' C.offset (S "The offset between the" +:+. (phrase targetPos `andThe` phrase landingPos))

targPosConcept :: ConceptChunk
targPosConcept = cc' targetPos $ foldlSent [S "The", phrase distance, S "from the", phrase launcher `toThe` phrase target]

launDurConcept :: ConceptChunk
launDurConcept = cc' launchDur $ foldlSent [S "The", phrase time, S "when the", phrase projectile, S "lands"]

---

isShort :: QuantityDict
isShort = vc "isShort"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase targetPos +:+
   S "is greater than the" +:+ phrase landingPos))
  (Atomic "isShort") Boolean

isHit :: QuantityDict
isHit = vc "isHit"
  (nounPhraseSent (S "variable that is assigned true when the" +:+ phrase landingPos +:+
   S "is within a degree of tolerance of the" +:+ phrase targetPos))
  (Atomic "isHit") Boolean
