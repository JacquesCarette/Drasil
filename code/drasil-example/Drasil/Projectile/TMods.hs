module Drasil.Projectile.TMods (tMods, accelerationTM, velocityTM) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)
import Data.Drasil.Quantities.Physics (acceleration, position, time, velocity)

import Drasil.Projectile.References (accelerationWiki, velocityWiki)

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM]

accelerationTM :: TheoryModel
accelerationTM = tm (cw accelerationRC)
  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk])
  [] [accelerationRel] [] [makeCite accelerationWiki] "acceleration" []

accelerationRC :: RelationConcept
accelerationRC = makeRC "accelerationRC" (cn' "acceleration") EmptyS accelerationRel

accelerationRel :: Relation
accelerationRel = sy acceleration $= deriv (sy velocity) time

----------

velocityTM :: TheoryModel
velocityTM = tm (cw velocityRC)
  [qw velocity, qw position, qw time] ([] :: [ConceptChunk])
  [] [velocityRel] [] [makeCite velocityWiki] "velocity" []

velocityRC :: RelationConcept
velocityRC = makeRC "velocityRC" (cn' "velocity") EmptyS velocityRel

velocityRel :: Relation
velocityRel = sy velocity $= deriv (sy position) time
