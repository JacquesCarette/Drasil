module Drasil.Projectile.Concepts where

import Language.Drasil
import Utils.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Math (angle)
import Data.Drasil.Concepts.Physics (position, speed)

concepts :: [NamedChunk]
concepts = [landingPos, launch, launchAngle, launchSpeed, launcher, projectile, target, targetPos]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

duration, landingPos, launch, launchAngle, launchDur, launchSpeed, launcher, projectile, target, targetPos :: NamedChunk
duration   = nc "duration"   (nounPhraseSP "duration")
launch     = nc "launch"     (nounPhraseSP "launch") -- FIXME: Used as adjective
launcher   = nc "launcher"   (nounPhraseSP "launcher")
projectile = nc "projectile" (nounPhraseSP "projectile")
target     = nc "target"     (nounPhraseSP "target")

landingPos  = compoundNC (nc "landing" (nounPhraseSP "landing")) position
launchAngle = compoundNC launch angle
launchDur   = compoundNC launch duration
launchSpeed = compoundNC launch speed
targetPos   = compoundNC target position
