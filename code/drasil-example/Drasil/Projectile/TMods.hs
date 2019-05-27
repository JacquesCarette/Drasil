module Drasil.Projectile.TMods (tMods) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Data.Drasil.Quantities.Physics (position, time, velocity)

tMods :: [TheoryModel]
tMods = [velocityTM]

------------- New Chunk -----------
velocityTM :: TheoryModel
velocityTM = tmNoRefs (cw velocityRC)
  [qw velocity, qw position, qw time] ([] :: [ConceptChunk])
  [] [velocityRel] [] "velocity" []

------------------------------------
velocityRC :: RelationConcept
velocityRC = makeRC "velocityRC" (cn' "velocity") EmptyS velocityRel

velocityRel :: Relation
velocityRel = (sy velocity) $= (sy position) / (sy time)
