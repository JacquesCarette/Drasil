module Drasil.Projectile.Concepts (
  launcher, projectile, target, projMotion, defs, rectVel, ideaDicts,
  flightDur, offset, landPos, launAngle, launSpeed, targPos, projSpeed, projPos
) where

import Drasil.Database (mkUid)
import Control.Lens ((^.))
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (oneD, position, speed, motion, distance, iSpeed, time,
  rectilinear, velocity, acceleration)

ideaDicts :: [IdeaDict]
ideaDicts = [projMotion, launch, rectVel]

launch :: IdeaDict
launch = idea' (mkUid "launch") (nounPhraseSP "launch")

rectVel :: IdeaDict
rectVel = compoundNC rectilinear velocity

projMotion :: IdeaDict
projMotion = compoundNC projectile motion
---

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

launcher, projectile, target, projSpeed, projPos, landPos, launAngle, launSpeed,
  offset, targPos, flightDur :: ConceptChunk
launcher   = cncpt''' (mkUid "launcher")        (nounPhraseSP "launcher")
  (S "where the projectile is launched from and the device that does the launching")
projectile = cncpt''' (mkUid "projectile")      (nounPhraseSP "projectile")
  (S "the object to be launched at the target")
target     = cncpt''' (mkUid "target")          (nounPhraseSP "target")
  (S "where the projectile should be launched to")
projSpeed  = cncpt''' (mkUid "projSpeed")       (nounPhraseSP "1D speed")
  (short oneD +:+ phrase speed +:+ S "under" +:+ phrase constant +:+ phrase acceleration)
projPos    = cncpt''' (mkUid "projPos")         (nounPhraseSP "1D position")
  (short oneD +:+ phrase position +:+ S "under" +:+ phrase constant +:+ phrase speed)
landPos    = cncpt''' (mkUid "landingposition") (compoundPhrase (nounPhraseSP "landing") (position ^. term))
  (foldlSent_ [D.toSent (phraseNP (the distance)) `S.fromThe` phrase launcher
    `S.toThe` S "final", D.toSent $ phraseNP (position `ofThe` projectile)])
launAngle  = cncpt''' (mkUid "launchangle")     (compoundPhrase (launch ^. term) (angle ^. term))
  (foldlSent_ [D.toSent $ phraseNP (the angle), S "between the", phrase launcher
    `S.and_` S "a straight line" `S.fromThe` D.toSent (phraseNP (launcher `toThe` target))])
launSpeed  = cncpt''' (mkUid "launchspeed")     (compoundPhrase (launch ^. term) (speed ^. term))
  (D.toSent (phraseNP (iSpeed `the_ofThe` projectile)) +:+ S "when launched")
offset     = cncpt''' (mkUid "offset")          (compoundPhrase (cn "distance between the") (targPos `andThe` landPos))
  (S "the offset between the" +:+ D.toSent (phraseNP (targPos `andThe` landPos)))
targPos    = cncpt''' (mkUid "targetposition")  (compoundPhrase (target ^. term) (position ^. term))
  (D.toSent (phraseNP (the distance)) `S.fromThe` D.toSent (phraseNP (launcher `toThe` target)))
flightDur  = cncpt''' (mkUid "flightduration")  (compoundPhrase (nounPhraseSP "flight") (nounPhraseSP "duration"))
  (foldlSent_ [D.toSent $ phraseNP (the time), S "when the", phrase projectile, S "lands"])
