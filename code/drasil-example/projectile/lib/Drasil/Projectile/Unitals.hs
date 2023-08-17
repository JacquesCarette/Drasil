module Drasil.Projectile.Unitals where

import Language.Drasil
import Language.Drasil.Display (Symbol(..))
import Language.Drasil.ShortHands (lD, lTheta, lV, lP, lT, lS, vEpsilon)

import Data.Drasil.Quantities.Math (pi_)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import qualified Drasil.Projectile.Concepts as C (flightDur, offset,
  flightDur, landPos, launAngle, launSpeed, offset, targPos, projSpeed, projPos)

projSpeed :: UnitalChunk
projSpeed = uc C.projSpeed (Concat [lV, label "(", lT, label ")"]) Real velU

projPos :: UnitalChunk
projPos = uc C.projPos (Concat [lP, label "(", lT, label ")"]) Real metre

---
landPosUnc, launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc,
  flightDurUnc :: UncertQ
landPosUnc   = uq landPos   defaultUncrt
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset    defaultUncrt
targPosUnc   = uq targPos   defaultUncrt
flightDurUnc = uq flightDur defaultUncrt

flightDur, landPos, launAngle, launSpeed, offset, targPos :: ConstrConcept
flightDur = constrainedNRV' (uc       C.flightDur (subStr lT "flight") Real second) [gtZeroConstr]
landPos   = constrainedNRV' (uc       C.landPos   (subStr lP "land"  ) Real metre ) [gtZeroConstr]
launAngle = constrained'    (ucStaged C.launAngle (autoStage lTheta  ) Real radian) [physc $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)
launSpeed = constrained'    (uc       C.launSpeed (subStr lV "launch") Real velU  ) [gtZeroConstr] (exactDbl 100)
offset    = constrainedNRV' (uc       C.offset    (subStr lD "offset") Real metre ) [physc $ UpFrom (Exc, neg $ sy targPos)]
targPos   = constrained'    (uc       C.targPos   (subStr lP "target") Real metre ) [gtZeroConstr] (exactDbl 1000)

---
-- The output contains a message, as a string, so it needs to be a quantity
message :: QuantityDict
message = vc "message" (nounPhraseSent (S "output message as a string")) lS String

---
tol :: ConstQDef
tol = mkQuantDef (vcSt "tol" (nounPhraseSP "hit tolerance") (autoStage vEpsilon) Real) (perc 2 2)
