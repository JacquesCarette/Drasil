module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.ShortHands

import Data.Drasil.Quantities.Math (pi_)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import qualified Drasil.Projectile.Concepts as C (flightDur, offset,
  flightDur, landPos, launAngle, launSpeed, offset, targPos)

---
landPosUnc, launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc :: UncertQ
landPosUnc   = uq landPos   defaultUncrt
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset    defaultUncrt
targPosUnc   = uq targPos   defaultUncrt

flightDur, landPos, launAngle, launSpeed, offset, targPos :: ConstrConcept
flightDur = constrainedNRV' (dqd  C.flightDur (subStr lT "flight") Real second)  [gtZeroConstr]
landPos   = constrainedNRV' (dqd  C.landPos   (subStr lP "land"  ) Real metre)   [gtZeroConstr]
launAngle = constrained'    (dqd' C.launAngle (autoStage lTheta)   Real (Just radian)) [physc $ Bounded (Exc, int 0) (Exc, sy pi_ $/ int 2)] (sy pi_ $/ int 4)
launSpeed = constrained'    (dqd  C.launSpeed (subStr lV "launch") Real velU)    [gtZeroConstr] (int 100)
offset    = constrainedNRV' (dqd  C.offset    (subStr lD "offset") Real metre)   [physc $ UpFrom (Exc, neg $ sy landPos) ]
targPos   = constrained'    (dqd  C.targPos   (subStr lP "target") Real metre)   [gtZeroConstr] (int 1000)

---
-- The output contains a message, as a string, so it needs to be a quantity
message :: QuantityDict
message = vc "message" (nounPhraseSent (S "output message as a string")) lS String

---
tol :: QDefinition
tol = mkQuantDef (vcSt "tol" (nounPhraseSP "hit tolerance") (autoStage vEpsilon) Rational) (Perc 2 2)
