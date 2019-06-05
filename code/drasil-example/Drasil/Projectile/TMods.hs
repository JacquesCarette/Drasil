module Drasil.Projectile.TMods (tMods, accelerationTM, velocityTM) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Data.Drasil.Quantities.Physics (acceleration, displacement, time, velocity)

tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM]

accelerationTM :: TheoryModel
accelerationTM = tmNoRefs (cw accelerationRC)
  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk])
  [] [accelerationRel] [] "acceleration" []

accelerationRC :: RelationConcept
accelerationRC = makeRC "accelerationRC" (cn' "acceleration") EmptyS accelerationRel

accelerationRel :: Relation
accelerationRel = sy acceleration $= deriv (sy velocity) time

----------

velocityTM :: TheoryModel
velocityTM = tmNoRefs (cw velocityRC)
  [qw velocity, qw displacement, qw time] ([] :: [ConceptChunk])
  [] [velocityRel] [] "velocity" []

velocityRC :: RelationConcept
velocityRC = makeRC "velocityRC" (cn' "velocity") EmptyS velocityRel

velocityRel :: Relation
velocityRel = sy velocity $= deriv (sy displacement) time
