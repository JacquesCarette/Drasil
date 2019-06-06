module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, speed)

concepts :: [NamedChunk]
concepts = [launch, launchAngle, launchDist, launchSpeed, launcher, projectile, targetDist, target]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

duration, launch, launchAngle, launchDist, launchDur, launchSpeed, launcher, projectile, targetDist, target :: NamedChunk
duration   = nc "duration"   (nounPhraseSP "duration")
launch     = nc "launch"     (nounPhraseSP "launch") -- FIXME: Used as adjective
launcher   = nc "launcher"   (nounPhraseSP "launcher")
projectile = nc "projectile" (nounPhraseSP "projectile")
target     = nc "target"     (nounPhraseSP "target")

launchAngle = compoundNC launch angle
launchDist  = compoundNC launch distance
launchDur   = compoundNC launch duration
launchSpeed = compoundNC launch speed
targetDist  = compoundNC target distance
