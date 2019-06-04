module Drasil.Projectile.Concepts where

import Language.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (distance, speed)
import Data.Drasil.Phrase (compoundNC)

concepts :: [NamedChunk]
concepts = [launch, launchAngle, launchDist, launchSpeed, launcher, projectile, targetDist, target]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

launch, launchAngle, launchDist, launchSpeed, launcher, projectile, targetDist, target :: NamedChunk
launch     = nc "launch"     (nounPhraseSP "launch") -- FIXME: Used as adjective
launcher   = nc "launcher"   (nounPhraseSP "launcher")
projectile = nc "projectile" (nounPhraseSP "projectile")
target     = nc "target"     (nounPhraseSP "target")

launchAngle = compoundNC launch angle
launchDist  = compoundNC launch distance
launchSpeed = compoundNC launch speed
targetDist  = compoundNC target distance
