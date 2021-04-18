module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed, motion, distance, iSpeed, time)

concepts :: [IdeaDict]
concepts = nw projMotion : map nw [landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC]
  ++ map nw defs

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

durationNC, flightDurNC, landingPosNC, launchNC, launchAngleNC, launchSpeedNC, offsetNC, targetPosNC :: NamedChunk
durationNC   = nc "duration" (nounPhraseSP "duration")
launchNC     = nc "launch"   (nounPhraseSP "launch")
offsetNC     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phrase targetPosNC `andThe` phrase landingPosNC)

flightDurNC   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) durationNC
landingPosNC  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngleNC = compoundNC launchNC angle
launchSpeedNC = compoundNC launchNC speed
targetPosNC   = compoundNC target position

projMotion :: NamedChunk
projMotion = compoundNC projectile motion
---

launcher, projectile, target :: ConceptChunk
launcher   = dcc "launcher"   (nounPhraseSP "launcher")  ("where the projectile is launched from " ++
                                                          "and the device that does the launching")
projectile = dcc "projectile" (nounPhraseSP "projectile") "the object to be launched at the target"
target     = dcc "target"     (nounPhraseSP "target")     "where the projectile should be launched to"

landPos, launAngle, launSpeed, offset, targPos, flightDur :: ConceptChunk
landPos = cc' landingPosNC
  (foldlSent_ [S "the", phrase distance, S "from the", phrase launcher, S "to",
            (S "final" +:+ phrase position) `ofThe` phrase projectile])

launAngle = cc' launchAngleNC
  (foldlSent_ [S "the", phrase angle, S "between the", phrase launcher `sAnd` S "a straight line",
             S "from the", phrase launcher `toThe` phrase target])

launSpeed = cc' launchSpeedNC (phrase iSpeed `ofThe` phrase projectile +:+ S "when launched")

offset = cc' offsetNC (S "the offset between the" +:+ phrase targetPosNC `andThe` phrase landingPosNC)

targPos = cc' targetPosNC
  (foldlSent_ [S "the", phrase distance, S "from the", phrase launcher `toThe` phrase target])

flightDur = cc' flightDurNC
  (foldlSent_ [S "the", phrase time, S "when the", phrase projectile, S "lands"])

