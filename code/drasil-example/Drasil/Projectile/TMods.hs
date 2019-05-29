module Drasil.Projectile.TMods (tMods) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Data.Drasil.Quantities.Physics (distance, scalarAccel, speed, time)

tMods :: [TheoryModel]
tMods = [speedTM, scalarAccelTM]

------------- New Chunk -----------
speedTM :: TheoryModel
speedTM = tmNoRefs (cw speedRC)
  [qw speed, qw distance, qw time] ([] :: [ConceptChunk])
  [] [speedRel] [] "speed" []

speedRC :: RelationConcept
speedRC = makeRC "speedRC" (cn' "speed") EmptyS speedRel

speedRel :: Relation
speedRel = sy speed $= deriv (sy distance) time

------------- New Chunk -----------
scalarAccelTM :: TheoryModel
scalarAccelTM = tmNoRefs (cw scalarAccelRC)
  [qw scalarAccel, qw speed, qw time] ([] :: [ConceptChunk])
  [] [scalarAccelRel] [] "scalarAccel" []

scalarAccelRC :: RelationConcept
scalarAccelRC = makeRC "scalarAccelRC" (cn' "scalar acceleration") EmptyS scalarAccelRel

scalarAccelRel :: Relation
scalarAccelRel = sy scalarAccel $= deriv (sy speed) time
