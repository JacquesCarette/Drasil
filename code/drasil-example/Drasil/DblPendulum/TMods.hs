module Drasil.DblPendulum.TMods (tMods, accelerationTM, velocityTM, newtonSL, newtonSLR) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tm, tmNoRefs)
import Data.Drasil.Quantities.Physics (acceleration, position, time, velocity, acceleration,
       momentOfInertia, angularAccel, torque)
import Data.Drasil.Concepts.Documentation (constant)
import Drasil.Projectile.References (accelerationWiki, velocityWiki, hibbeler2004)
import Data.Drasil.Theories.Physics (newtonSL)
import Data.Drasil.Concepts.Physics (pendulum)
import Drasil.DblPendulum.Assumptions (pend2DMotion)
import Utils.Drasil



-----------
tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM, newtonSL, newtonSLR]

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
--Newton's second Law of rotation--------------------------

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (cw newtonSLRRC)
  [qw torque, qw momentOfInertia, qw angularAccel] 
  ([] :: [ConceptChunk]) [] [newtonSLRRel] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRRC :: RelationConcept
newtonSLRRC = makeRC "newtonSLRRC" 
  (nounPhraseSP "Newton's second law for rotational motion") EmptyS newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = sy torque $= sy momentOfInertia * sy angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS torque, S "on a", phrase pendulum `sIs`
   S "proportional to its", getTandS angularAccel `sC` S "where",
   ch momentOfInertia, S "denotes", phrase momentOfInertia `ofThe`
   phrase pendulum, S "as the", phrase constant `sOf` S "proportionality",
   S "We also assume that pendulum motion is two-dimensional" +:+ makeRef2S pend2DMotion]]

   --------------------------
  