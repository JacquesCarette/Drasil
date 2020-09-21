module Drasil.DblPendulum.TMods (tMods, accelerationTM, velocityTM, newtonSL) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm)
import Data.Drasil.Quantities.Physics (acceleration, position, time, velocity, acceleration)
--import Data.Drasil.Quantities.PhysicalProperties (mass)
import Drasil.Projectile.References (accelerationWiki, velocityWiki, hibbeler2004)
import Data.Drasil.Theories.Physics (newtonSL)

-----------
tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM, newtonSL]

accelerationTM :: TheoryModel
accelerationTM = tm (cw accelerationRC)
  [qw acceleration, qw velocity, qw time] ([] :: [ConceptChunk]) [] [accelerationRel] []
  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "acceleration" []

accelerationRC :: RelationConcept
accelerationRC = makeRC "accelerationRC" (cn' "acceleration") EmptyS accelerationRel

accelerationRel :: Relation
accelerationRel = sy acceleration $= deriv (sy velocity) time

----------

velocityTM :: TheoryModel
velocityTM = tm (cw velocityRC)
  [qw velocity, qw position, qw time] ([] :: [ConceptChunk]) [] [velocityRel] []
  [makeCite velocityWiki, makeCiteInfo hibbeler2004 $ Page [6]] "velocity" []

velocityRC :: RelationConcept
velocityRC = makeRC "velocityRC" (cn' "velocity") EmptyS velocityRel

velocityRel :: Relation
velocityRel = sy velocity $= deriv (sy position) time

-----------------
-- newtonsLMTM :: TheoryModel --Newton's law of motion
-- newtonsLMTM = tm (cw newtonsLMRC)
--  [qw force, qw mass, qw acceleration] ([] :: [ConceptChunk]) [] [newtonsLMRel] []
--  [makeCite accelerationWiki, makeCiteInfo hibbeler2004 $ Page [7]] "newton's law of motion" []

-- newtonsLMRC :: RelationConcept
-- newtonsLMRC = makeRC "newtonsLMRC" (nounPhraseSP "Newton's first law of motion") EmptyS newtonsLMRel

-- newtonsLMRel :: Relation
-- newtonsLMRel = sy acceleration $= sy acceleration * sy acceleration