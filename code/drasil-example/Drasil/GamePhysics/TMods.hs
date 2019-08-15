module Drasil.GamePhysics.TMods (tMods) where

import Language.Drasil
import Control.Lens ((^.))
import Theory.Drasil (TheoryModel, tmNoRefs)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dispUnit, force_1, force_2,
  mass_1, mass_2, sqrDist)

import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (angularAccel, 
  displacement, force, gravitationalConst, momentOfInertia, torque)
import qualified Data.Drasil.Theories.Physics as TP (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [TP.newtonSL, newtonTL, newtonLUG, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (cw newtonTLRC)
  [qw force_1, qw force_2] ([] :: [ConceptChunk])
  [] [sy force_1 $= negate (sy force_2)] []
  "NewtonThirdLawMot" [newtonTLDesc]

newtonTLRC :: RelationConcept
newtonTLRC = makeRC "newtonTLRC" (nounPhraseSP "Newton's third law of motion")
  newtonTLDesc newtonTLRel

newtonTLRel :: Relation
newtonTLRel = sy force_1 $= negate (sy force_2)

newtonTLDesc :: Sentence
newtonTLDesc = foldlSent [S "Every action has an equal and opposite reaction. In other",
  S "words, the", phrase QP.force, ch force_1,
  sParen $ Sy $ unit_symb force_1, S "exerted on the second",
  phrase CP.rigidBody, S "by the first is equal in magnitude and",
  S "in the opposite direction to the", phrase QP.force,
  ch force_2, sParen $ Sy $ unit_symb force_2,
  S "exerted on the first", phrase CP.rigidBody, S "by the second"]

-- T3 : Newton's law of universal gravitation --

newtonLUG :: TheoryModel
newtonLUG = tmNoRefs (cw newtonLUGRC)
  [qw QP.force, qw QP.gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dispUnit, qw QP.displacement] ([] :: [ConceptChunk])
  [] [sy QP.force $= sy QP.gravitationalConst * (sy mass_1 * 
  sy mass_2 / (sy dispNorm $^ 2)) * sy dispUnit $= 
  sy QP.gravitationalConst * (sy mass_1 * sy mass_2 / (sy dispNorm
  $^ 2)) * (sy QP.displacement / sy dispNorm)] []
  "UniversalGravLaw" [newtonLUGDesc]

newtonLUGRC :: RelationConcept
newtonLUGRC = makeRC "newtonLUGRC" 
  (nounPhraseSP "Newton's law of universal gravitation") newtonLUGDesc newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = sy QP.force $=
  sy QP.gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * sy dispUnit $=
  sy QP.gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * (sy QP.displacement / sy dispNorm)

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187)).

newtonLUGDesc :: Sentence
newtonLUGDesc = foldlSent [S "Two", plural CP.rigidBody, S "in the universe",
  S "attract each other with a", phrase QP.force, 
  ch QP.force, sParen $ Sy $ unit_symb QP.force,
  S "that is directly proportional to the product of their",
  plural QPP.mass `sC` ch mass_1 `sAnd`
  ch mass_2, sParen (Sy $ unit_symb QPP.mass) `sC` S "and",
  S "inversely proportional to the", phrase sqrDist,
  ch sqrDist, sParen $ Sy $ unit_symb sqrDist,
  S "between them. The vector", ch QP.displacement,
  sParen $ Sy $ unit_symb QP.displacement, S "is the", 
  phrase QP.displacement, S "between the centres of the", 
  plural CP.rigidBody, S "and", ch dispNorm, 
  sParen $ Sy $ unit_symb dispNorm, S "represents the" +:+. 
  (phrase dispNorm `sC` S "or absolute distance between the two"), 
  ch dispUnit, S "denotes the", phrase dispUnit `sC`
  S "equivalent to the", phrase QP.displacement,
  S "divided by the" +:+. (phrase dispNorm `sC`
  S "as shown above"), S "Finally" `sC` ch QP.gravitationalConst `sIs`
  (QP.gravitationalConst ^. defn), sParen $ Sy $ unit_symb QP.gravitationalConst]

-- T4 : Newton's second law for rotational motion --

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (cw newtonSLRRC)
  [qw QP.torque, qw QP.momentOfInertia, qw QP.angularAccel] 
  ([] :: [ConceptChunk]) [] [sy QP.torque $= sy QP.momentOfInertia
  * sy QP.angularAccel] [] "NewtonSecLawRotMot" [newtonSLRDesc]

newtonSLRRC :: RelationConcept
newtonSLRRC = makeRC "newtonSLRRC" 
  (nounPhraseSP "Newton's second law for rotational motion") newtonSLRDesc newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = sy QP.torque $= sy QP.momentOfInertia * sy QP.angularAccel

-- Need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section.
newtonSLRDesc :: Sentence
newtonSLRDesc = foldlSent [S "The net", phrase QP.torque,
  ch QP.torque,
  sParen $ Sy $ unit_symb QP.torque, S "on a", phrase CP.rigidBody,
  S "is proportional to its", phrase QP.angularAccel,
  ch QP.angularAccel +:+. sParen (Sy $ unit_symb QP.angularAccel),
  S "Here" `sC` ch QP.momentOfInertia,
  sParen $ Sy $ unit_symb QP.momentOfInertia,
  S "denotes the", phrase QP.momentOfInertia, S "of the" +:+.
  phrase CP.rigidBody, S "We also assume that all",
  plural CP.rigidBody, S "involved are two-dimensional",
  makeRef2S assumpOD]

