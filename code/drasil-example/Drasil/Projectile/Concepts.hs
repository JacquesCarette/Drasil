module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

concepts :: [IdeaDict]
concepts = map nw [landingPos, launch, launchAngle, launchSpeed, offset, targetPos]
  ++ map nw defs

defs :: [ConceptChunk]
defs = [launcher, projectile, target]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

duration, flightDur, landingPos, launch, launchAngle, launchSpeed, offset, targetPos :: NamedChunk
duration   = nc "duration" (nounPhraseSP "duration")
launch     = nc "launch"   (nounPhraseSP "launch") -- FIXME: Used as adjective
offset     = nc "offset"   (nounPhraseSent $ S "distance between the" +:+ phrase targetPos `andThe` phrase landingPos)

flightDur   = compoundNC (nc "flight"  (nounPhraseSP "flight" )) duration
landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngle = compoundNC launch angle
launchSpeed = compoundNC launch speed
targetPos   = compoundNC target position

---

launcher, projectile, target :: ConceptChunk
launcher   = dcc "launcher"   (nounPhraseSP "launcher")  ("where the projectile is launched from " ++
                                                          "and the device that does the launching")
projectile = dcc "projectile" (nounPhraseSP "projectile") "the object to be launched at the target"
target     = dcc "target"     (nounPhraseSP "target")     "where the projectile should be launched to"
