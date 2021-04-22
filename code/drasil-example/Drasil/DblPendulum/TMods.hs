module Drasil.DblPendulum.TMods (tMods, accelerationTM, velocityTM, newtonSL, newtonSLR) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Data.Drasil.Quantities.Physics (momentOfInertia, angularAccel, torque)
import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Theories.Physics (newtonSL, accelerationTM, velocityTM)
import Data.Drasil.Concepts.Physics (pendulum)
import Drasil.DblPendulum.Assumptions (pend2DMotion)
import Utils.Drasil

-----------
tMods :: [TheoryModel]
tMods = [accelerationTM, velocityTM, newtonSL, newtonSLR]

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
  
