module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

concepts :: [IdeaDict]
concepts = map nw [landingPos, launch, launchAngle, launchSpeed, launcher,
    offset, projectile, target, targetPos] ++ map nw messages

messages :: [ConceptChunk]
messages = [hitMessage, shortMessage, longMessage]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

duration, landingPos, launch, launchAngle, launchDur, launchSpeed, launcher, offset,
    projectile, target, targetPos :: NamedChunk
duration   = nc "duration"   (nounPhraseSP "duration")
launch     = nc "launch"     (nounPhraseSP "launch") -- FIXME: Used as adjective
launcher   = nc "launcher"   (nounPhraseSP "launcher")
offset     = nc "offset"     (nounPhraseSent $ S "offset between the" +:+ phrase targetPos `andThe` phrase landingPos)
projectile = nc "projectile" (nounPhraseSP "projectile")
target     = nc "target"     (nounPhraseSP "target")

landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngle = compoundNC launch angle
launchDur   = compoundNC launch duration
launchSpeed = compoundNC launch speed
targetPos   = compoundNC target position

---
hitMessage, shortMessage, longMessage :: ConceptChunk
hitMessage   = dcc "hitMessage"   (nounPhraseSP "hit")   "The target was hit."
shortMessage = dcc "shortMessage" (nounPhraseSP "short") "The projectile fell short."
longMessage  = dcc "longMessage"  (nounPhraseSP "long")  "The projectile went long."
