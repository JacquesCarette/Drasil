module Drasil.Projectile.Concepts where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (oneD, position, speed, motion, distance, iSpeed, time,
  rectilinear, velocity, acceleration)

concepts :: [IdeaDict]
concepts = nw projMotion : map nw [landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC,
  rectVel] ++ map nw defs

durationNC, flightDurNC, landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC,
  rectVel :: NamedChunk
durationNC   = nc "duration" (nounPhraseSP "duration")
launchNC     = nc "launch"   (nounPhraseSP "launch")
offsetNC     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phraseNP (targetPosNC `andThe` landingPosNC))

flightDurNC   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) durationNC
landingPosNC  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngleNC = compoundNC launchNC angle
launchSpeedNC = compoundNC launchNC speed
targetPosNC   = compoundNC target position
rectVel       = compoundNC rectilinear velocity

projMotion :: NamedChunk
projMotion = compoundNC projectile motion
---

defs :: [ConceptChunk]
defs = [launcher, projectile, target, projSpeed, projPos]

launcher, projectile, target, projSpeed, projPos :: ConceptChunk
launcher   = dcc "launcher"   (nounPhraseSP "launcher")  ("where the projectile is launched from " ++
                                                          "and the device that does the launching")
projectile = dcc "projectile" (nounPhraseSP "projectile") "the object to be launched at the target"
target     = dcc "target"     (nounPhraseSP "target")     "where the projectile should be launched to"

projSpeed  = dccWDS "projSpeed" (nounPhraseSP "1D speed")    (getAcc oneD +:+ phrase speed +:+ S "under" +:+ phrase constant +:+ phrase acceleration)
projPos    = dccWDS "projPos"   (nounPhraseSP "1D position") (getAcc oneD +:+ phrase position +:+ S "under" +:+ phrase constant +:+ phrase speed)

landPos, launAngle, launSpeed, offset, targPos, flightDur :: ConceptChunk
landPos = cc' landingPosNC
  (foldlSent_ [phraseNP (the distance) `S.fromThe` phrase launcher `S.toThe`
            S "final", phraseNP (position `ofThe` projectile)])

launAngle = cc' launchAngleNC
  (foldlSent_ [phraseNP (the angle), S "between the", phrase launcher `S.and_` S "a straight line"
             `S.fromThe` phraseNP (launcher `toThe` target)])

launSpeed = cc' launchSpeedNC (phraseNP (iSpeed `the_ofThe` projectile) +:+ S "when launched")
offset = cc' offsetNC (S "the offset between the" +:+ phraseNP (targetPosNC `andThe` landingPosNC))
targPos = cc' targetPosNC (phraseNP (the distance) `S.fromThe` phraseNP (launcher `toThe` target))
flightDur = cc' flightDurNC (foldlSent_ [phraseNP (the time), S "when the", phrase projectile, S "lands"])

