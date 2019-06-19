module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

concepts :: [IdeaDict]
concepts = map nw [landingPos, launch, launchAngle, launchSpeed, offset, targetPos]
  ++ map nw defs ++ map nw messages

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

messages :: [ConceptChunk]
messages = [hitMessage, shortMessage, longMessage]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

duration, flightDur, landingPos, launch, launchAngle, launchSpeed, offset, targetPos :: NamedChunk
duration   = nc "duration" (nounPhraseSP "duration")
launch     = nc "launch"   (nounPhraseSP "launch") -- FIXME: Used as adjective
offset     = nc "offset"   (nounPhraseSent $ S "offset between the" +:+ phrase targetPos `andThe` phrase landingPos)

flightDur   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) duration
landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngle = compoundNC launch angle
launchSpeed = compoundNC launch speed
targetPos   = compoundNC target position

---
hitMessage, launcher, longMessage, projectile, shortMessage, target :: ConceptChunk
launcher   = dcc "launcher"   (nounPhraseSP "launcher")   "Where the projectile is launched from"
projectile = dcc "projectile" (nounPhraseSP "projectile") "The object to be launched at the target"
target     = dcc "target"     (nounPhraseSP "target")     "Where the projectile should be launched to"

hitMessage   = dcc "hitMessage"   (nounPhraseSP "hit")   "The target was hit."
shortMessage = dcc "shortMessage" (nounPhraseSP "short") "The projectile fell short."
longMessage  = dcc "longMessage"  (nounPhraseSP "long")  "The projectile went long."
