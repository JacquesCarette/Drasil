module Drasil.Projectile.Concepts where

import Language.Drasil

import Data.Drasil.IdeaDicts (physics)

import Data.Drasil.Concepts.Physics (distance)
import Data.Drasil.Phrase (compoundNC)

concepts :: [NamedChunk]
concepts = [launcher, projectile, targetDist, target]

projectileTitle :: CI
projectileTitle = commonIdeaWithDict "projectileTitle" (pn "Projectile") "Projectile" [physics]

launcher, projectile, targetDist, target :: NamedChunk
launcher   = nc "launcher"   (nounPhraseSP "launcher")
projectile = nc "projectile" (nounPhraseSP "projectile")
target     = nc "target"     (nounPhraseSP "target")

targetDist = compoundNC target distance
