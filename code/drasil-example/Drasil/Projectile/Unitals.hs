module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands
import Data.Drasil.IdeaDicts
import Theory.Drasil (mkQuantDef)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, physSyst,
  requirement, srs, typUnc)
import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, oneD, twoD)

import Data.Drasil.Quantities.Physics (acceleration, constAccel,
  gravitationalAccelConst, iPos, iSpeed, iVel, ixPos, iyPos, ixVel, iyVel,
  position, scalarPos, speed, time, velocity, xAccel, xConstAccel, xPos,
  xVel, yAccel, yConstAccel, yPos, yVel)
import Data.Drasil.Quantities.Math (pi_, piConst)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import Drasil.Projectile.Concepts (landingPos, launcher, launchAngle,
  launchSpeed, projectile, targetPos, target)
import qualified Drasil.Projectile.Concepts as C (flightDur, offset)

symbols :: [QuantityDict]
symbols = qw gravitationalAccelConst : unitalQuants ++ map qw constants ++
  map qw [acceleration, constAccel, iPos, iSpeed, iVel, ixPos,
  iyPos, ixVel, iyVel, position, scalarPos, speed, time, velocity, xAccel,
  xConstAccel, xPos, xVel, yAccel, yConstAccel, yPos, yVel]

-- FIXME: Move to Defs?
acronyms :: [CI]
acronyms = [oneD, twoD, assumption, dataDefn, genDefn, goalStmt, inModel,
  physSyst, requirement, srs, thModel, typUnc]

constants :: [QDefinition]
constants = [gravitationalAccelConst, piConst, tol]

inputs :: [QuantityDict]
inputs = map qw [launSpeed, launAngle, targPos]

outputs :: [QuantityDict]
outputs = [message, qw offset]

unitalQuants :: [QuantityDict]
unitalQuants = message : map qw constrained

unitalIdeas :: [IdeaDict]
unitalIdeas = nw message : map nw constrained

inConstraints :: [UncertQ]
inConstraints = [launAngleUnc, launSpeedUnc, targPosUnc]

outConstraints :: [UncertQ]
outConstraints = [landPosUnc, offsetUnc]

constrained :: [ConstrConcept]
constrained = [flightDur, landPos, launAngle, launSpeed, offset, targPos]

---
landPosUnc, launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc :: UncertQ
landPosUnc   = uq landPos   defaultUncrt
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset    defaultUncrt
targPosUnc   = uq targPos   defaultUncrt

flightDur, landPos, launAngle, launSpeed, offset, targPos :: ConstrConcept
flightDur = constrainedNRV' (dqd  flightDurConcept (unitHelper lT "flight") Real second)  [gtZeroConstr]
landPos   = constrainedNRV' (dqd  landPosConcept   (unitHelper lP "land"  ) Real metre)   [gtZeroConstr]
launAngle = constrained'    (dqd' launAngleConcept (autoStage lTheta) Real (Just radian)) [physc $ Bounded (Exc, 0) (Exc, sy pi_ / 2)] (sy pi_ / 4)
launSpeed = constrained'    (dqd  launSpeedConcept (unitHelper lV "launch") Real velU)    [gtZeroConstr] (int 100)
offset    = constrainedNRV' (dqd  offsetConcept    (unitHelper lD "offset") Real metre)   [physc $ UpFrom (Exc, negate $ sy landPos) ]
targPos   = constrained'    (dqd  targPosConcept   (unitHelper lP "target") Real metre)   [gtZeroConstr] (int 1000)

unitHelper :: Symbol -> String -> Symbol
unitHelper sym substr = sub sym $ Label substr

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

flightDurConcept :: ConceptChunk
flightDurConcept = cc' C.flightDur $ foldlSent [S "The", phrase time, S "when the", phrase projectile, S "lands"]

---
message :: QuantityDict
message = vc "message" (nounPhraseSent (S "output message as a string")) lS String

---
tol :: QDefinition
tol = mkQuantDef (vc "tol" (nounPhraseSP "hit tolerance") vEpsilon Rational) (Perc 2 2)
