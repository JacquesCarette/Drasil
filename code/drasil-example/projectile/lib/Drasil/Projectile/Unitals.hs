module Drasil.Projectile.Unitals (
  offset, flightDur, targPos, inputs, outputs,
  launAngle, launSpeed, landPos, projSpeed, projPos,
  launAngleUnc, launSpeedUnc, targPosUnc, landPosUnc, offsetUnc, flightDurUnc,
  flightDurCon
) where

import Control.Lens ((^.))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Drasil.Database (mkUid)
import Language.Drasil.Display (Symbol(Concat))
import Language.Drasil.ShortHands (lD, lTheta, lV, lP, lT)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Physics (distance, oneD)
import Data.Drasil.Quantities.Physics (acceleration, iSpeed, position, speed, time)
import Data.Drasil.Quantities.Math (pi_)

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (radian, metre, second)
import Data.Drasil.Units.Physics (velU)

import qualified Drasil.Projectile.Concepts as C (launAngleTerm, launAngleDef, launch,
  launcher, projectile, target)

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = dqdWr <$> launSpeed :| [launAngle, targPos]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = dqdWr <$> offset :| [flightDurCon]

projSpeed :: DefinedQuantityDict
projSpeed = quant (mkUid "projSpeed")
  (compoundPhrase (nounPhraseSP (abrv oneD)) (speed ^. term))
  (short oneD +:+ phrase speed +:+ S "under" +:+ phrase constant +:+ phrase acceleration)
  (Concat [lV, label "(", lT, label ")"]) Real velU

projPos :: DefinedQuantityDict
projPos = quant (mkUid "projPos")
  (compoundPhrase (nounPhraseSP (abrv oneD)) (position ^. term))
  (short oneD +:+ phrase position +:+ S "under" +:+ phrase constant +:+ phrase speed)
  (Concat [lP, label "(", lT, label ")"]) Real metre

---
landPosUnc, launAngleUnc, launSpeedUnc, offsetUnc, targPosUnc,
  flightDurUnc :: UncertQ
landPosUnc   = uq landPos defaultUncrt
launAngleUnc = uq launAngle defaultUncrt
launSpeedUnc = uq launSpeed defaultUncrt
offsetUnc    = uq offset defaultUncrt
targPosUnc   = uq targPos defaultUncrt
flightDurUnc = uq flightDurCon defaultUncrt

flightDur :: DefinedQuantityDict
flightDur = quant (mkUid "flightduration")
  (compoundPhrase (nounPhraseSP "flight") (nounPhraseSP "duration"))
  (foldlSent_ [D.toSent $ phraseNP (the time), S "when the", phrase C.projectile, S "lands"])
  (subStr lT "flight") Real second

flightDurCon, landPos, launAngle, launSpeed, offset, targPos :: ConstrConcept
flightDurCon = constrainedNRV' flightDur [gtZeroConstr]
landPos   = constrainedNRV' (quant (mkUid "landingposition")
  (compoundPhrase (nounPhraseSP "landing") (position ^. term))
  (foldlSent_ [D.toSent (phraseNP (the distance)) `S.fromThe` phrase C.launcher
    `S.toThe` S "final", D.toSent $ phraseNP (position `ofThe` C.projectile)])
  (subStr lP "land") Real metre) [gtZeroConstr]
launAngle = constrained' (quant' (mkUid "launchangle") C.launAngleTerm C.launAngleDef
              (autoStage lTheta) Real radian)
              [physRange $ Bounded (Exc, exactDbl 0) (Exc, half $ sy pi_)] (sy pi_ $/ exactDbl 4)
launSpeed = constrained' (quant (mkUid "launchspeed")
  (compoundPhrase (C.launch ^. term) (speed ^. term))
  (D.toSent (phraseNP (iSpeed `the_ofThe` C.projectile)) +:+ S "when launched")
  (subStr lV "launch") Real velU) [gtZeroConstr] (exactDbl 100)
offset    = constrainedNRV' (quant (mkUid "offset")
  (compoundPhrase (cn "distance between the") (targPos `andThe` landPos))
  (S "the offset between the" +:+ D.toSent (phraseNP (targPos `andThe` landPos)))
  (subStr lD "offset") Real metre) [physRange $ UpFrom (Exc, neg $ sy targPos)]
targPos   = constrained' (quant (mkUid "targetposition")
  (compoundPhrase (C.target ^. term) (position ^. term))
  (D.toSent (phraseNP (the distance)) `S.fromThe` D.toSent (phraseNP (C.launcher `toThe` C.target)))
  (subStr lP "target") Real metre) [gtZeroConstr] (exactDbl 1000)
