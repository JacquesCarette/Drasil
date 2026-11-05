module Drasil.Projectile.Unitals (
  offset, flightDur, targPos, inputs, outputs,
  launAngle, launSpeed, landPos, projSpeed, projPos,
  launAngleUnc, launSpeedUnc, targPosUnc, landPosUnc, offsetUnc, flightDurUnc
) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Drasil.Database (mkUid)
import Language.Drasil.Display (Symbol(Concat))
import Language.Drasil.ShortHands (lD, lTheta, lV, lP, lT)

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Physics (oneD, position, speed, acceleration)
import Data.Drasil.Quantities.Math (pi_)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import qualified Drasil.Projectile.Concepts as C (flightDur, offset,
  flightDur, landPos, launAngleTerm, launAngleDef, launSpeed, offset, targPos)

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = dqdWr <$> launSpeed :| [launAngle, targPos]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = dqdWr <$> offset :| [flightDur]

projSpeed :: DefinedQuantityDict
projSpeed = quant (mkUid "projSpeed") (nounPhraseSP "1D speed")
  (short oneD +:+ phrase speed +:+ S "under" +:+ phrase constant +:+ phrase acceleration)
  (Concat [lV, label "(", lT, label ")"]) Real velU

projPos :: DefinedQuantityDict
projPos = quant (mkUid "projPos") (nounPhraseSP "1D position")
  (short oneD +:+ phrase position +:+ S "under" +:+ phrase constant +:+ phrase speed)
  (Concat [lP, label "(", lT, label ")"]) Real metre

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
flightDur = constrainedNRV' (dqd      C.flightDur (subStr lT "flight") Real second) [gtZeroConstr]
landPos   = constrainedNRV' (dqd      C.landPos   (subStr lP "land"  ) Real metre ) [gtZeroConstr]
launAngle = constrained' (quant' (mkUid "launchangle") C.launAngleTerm C.launAngleDef
              (autoStage lTheta) Real radian)
              [physRange $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)
launSpeed = constrained'    (dqd      C.launSpeed (subStr lV "launch") Real velU  ) [gtZeroConstr] (exactDbl 100)
offset    = constrainedNRV' (dqd      C.offset    (subStr lD "offset") Real metre ) [physRange $ UpFrom (Exc, neg $ sy targPos)]
targPos   = constrained'    (dqd      C.targPos   (subStr lP "target") Real metre ) [gtZeroConstr] (exactDbl 1000)
