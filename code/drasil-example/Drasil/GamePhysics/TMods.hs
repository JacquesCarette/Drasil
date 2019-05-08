module Drasil.GamePhysics.TMods (cpTMods, t1NewtonSL_new, t2NewtonTL_new, 
t3NewtonLUG_new, t4ChaslesThm_new, t5NewtonSLR_new, cpTModsNew) where

import Language.Drasil
import Prelude hiding (id)
import Control.Lens ((^.))

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dispUnit, force_1, force_2,
  mass_1, mass_2, rOB, sqrDist, velB, velO)

import Data.Drasil.SentenceStructures (foldlSent)
import qualified Data.Drasil.Concepts.Physics as CP (rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (mass)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  angularAccel, angularVelocity, displacement, force, gravitationalConst, 
  momentOfInertia, torque, velocity)

----- Theoretical Models -----

cpTMods :: [RelationConcept]
cpTMods = [newtonSL, newtonTL, newtonLUG, chaslesThm, newtonSLR]

cpTModsNew :: [TheoryModel]
cpTModsNew = [t1NewtonSL_new, t2NewtonTL_new, t3NewtonLUG_new, 
  t4ChaslesThm_new, t5NewtonSLR_new]

-- T1 : Newton's second law of motion --

t1NewtonSL_new :: TheoryModel
t1NewtonSL_new = tm (cw newtonSL)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [] [(sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration)] [] [] 
  "NewtonSecLawMot" [newtonSLDesc]

newtonSL :: RelationConcept
newtonSL = makeRC "newtonSL" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration)

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", (phrase QP.force), (ch QP.force),
  (sParen $ Sy $ unit_symb QP.force), S "on a", (phrase CP.rigidBody),
  S "is proportional to the", (phrase QP.acceleration),
  (ch QP.acceleration), (sParen $ Sy $ unit_symb QP.acceleration),
  S "of the", (phrase CP.rigidBody) `sC`
  S "where", (ch QPP.mass), (sParen $ Sy $ unit_symb QPP.mass),
  S "denotes the", (phrase QPP.mass), S "of the",
  (phrase $ CP.rigidBody),
  S "as the constant of proportionality"]

-- T2 : Newton's third law of motion --

t2NewtonTL_new :: TheoryModel
t2NewtonTL_new = tm (cw newtonTL)
  [qw force_1, qw force_2] ([] :: [ConceptChunk])
  [] [(sy force_1) $= (negate (sy force_2))] [] [] 
  "NewtonThirdLawMot" [newtonTLDesc]

newtonTL :: RelationConcept
newtonTL = makeRC "newtonTL" (nounPhraseSP "Newton's third law of motion")
  newtonTLDesc newtonTLRel

newtonTLRel :: Relation
newtonTLRel = (sy force_1) $= (negate (sy force_2))

newtonTLDesc :: Sentence
newtonTLDesc = foldlSent [S "Every action has an equal and opposite reaction. In other",
  S "words, the", (phrase QP.force), (ch force_1),
  (sParen $ Sy $ unit_symb force_1), S "exerted on the second",
  (phrase CP.rigidBody), S "by the first is equal in magnitude and",
  S "in the opposite direction to the", (phrase QP.force),
  (ch force_2), (sParen $ Sy $ unit_symb force_2),
  S "exerted on the first", (phrase CP.rigidBody), S "by the second"]

-- T3 : Newton's law of universal gravitation --

t3NewtonLUG_new :: TheoryModel
t3NewtonLUG_new = tm (cw newtonLUG)
  [qw QP.force, qw QP.gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dispUnit, qw QP.displacement] ([] :: [ConceptChunk])
  [] [(sy QP.force) $= (sy QP.gravitationalConst) * ((sy mass_1) * 
  (sy mass_2) / ((sy dispNorm) $^ 2)) * (sy dispUnit) $= 
  (sy QP.gravitationalConst) * ((sy mass_1) * (sy mass_2) / ((sy dispNorm) 
  $^ 2)) * ((sy QP.displacement) / (sy dispNorm))] [] [] 
  "UniversalGravLaw" [newtonLUGDesc]

newtonLUG :: RelationConcept
newtonLUG = makeRC "newtonLUG" 
  (nounPhraseSP "Newton's law of universal gravitation") newtonLUGDesc newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = (sy QP.force) $=
  (sy QP.gravitationalConst) * ((sy mass_1) * (sy mass_2) /
  ((sy dispNorm) $^ 2)) * (sy dispUnit) $=
  (sy QP.gravitationalConst) * ((sy mass_1) * (sy mass_2) /
  ((sy dispNorm) $^ 2)) * ((sy QP.displacement) / (sy dispNorm))

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187)).

newtonLUGDesc :: Sentence
newtonLUGDesc = foldlSent [S "Two", (plural CP.rigidBody), S "in the universe",
  S "attract each other with a", (phrase QP.force), 
  (ch QP.force), (sParen $ Sy $ unit_symb QP.force),
  S "that is directly proportional to the product of their",
  (plural QPP.mass) `sC` (ch mass_1), S "and",
  (ch mass_2), (sParen $ Sy $ unit_symb QPP.mass) `sC` S "and",
  S "inversely proportional to the", (phrase $ sqrDist),
  (ch sqrDist), (sParen $ Sy $ unit_symb sqrDist),
  S "between them. The vector", (ch QP.displacement), 
  (sParen $ Sy $ unit_symb QP.displacement), S "is the", 
  (phrase QP.displacement), S "between the centres of the", 
  (plural CP.rigidBody), S "and", (ch dispNorm), 
  (sParen $ Sy $ unit_symb dispNorm), S "represents the" +:+. 
  ((phrase dispNorm) `sC` S "or absolute distance between the two"), 
  (ch dispUnit), S "denotes the", (phrase $ dispUnit) `sC` 
  S "equivalent to the", (phrase QP.displacement), 
  S "divided by the" +:+. ((phrase dispNorm) `sC`
  S "as shown above"), S "Finally" `sC` (ch QP.gravitationalConst), 
  S "is the", (QP.gravitationalConst ^. defn), 
  (sParen $ Sy $ unit_symb QP.gravitationalConst)]

-- T4 : Chasles' theorem --

t4ChaslesThm_new :: TheoryModel
t4ChaslesThm_new = tm (cw chaslesThm)
  [qw velB, qw velO, qw QP.angularVelocity, qw rOB] 
  ([] :: [ConceptChunk]) [] [(sy velB) $= (sy velO) + (cross 
  (sy  QP.angularVelocity) (sy rOB))] [] [] "ChaslesThm" [chaslesThmDesc]

chaslesThm :: RelationConcept
chaslesThm = makeRC "chaslesThm" (nounPhraseSP "Chasles' theorem")
  chaslesThmDesc chaslesThmRel

-- Need the cross product symbol - third term should be a cross product.
chaslesThmRel :: Relation
chaslesThmRel = (sy velB) $= (sy velO) + (cross (sy  QP.angularVelocity) (sy rOB))

-- B should ideally be italicized in 'point B' (line 202).
chaslesThmDesc :: Sentence
chaslesThmDesc = foldlSent [S "The linear", (phrase QP.velocity),
  (ch velB), (sParen $ Sy $ unit_symb velB), S "of any point B in a",
  (phrase CP.rigidBody), makeRef2S assumpOT, S "is the sum of the linear",
  (phrase QP.velocity), (ch velO),
  (sParen $ Sy $ unit_symb velO), S "of the", (phrase $ CP.rigidBody),
  S "at the origin (axis of rotation) and the",
  S "resultant vector from the cross product of the",
  (phrase CP.rigidBody) :+: S "'s", (phrase QP.angularVelocity), 
  (ch QP.angularVelocity), 
  (sParen $ Sy $ unit_symb  QP.angularVelocity), S "and the", 
  (phrase rOB) `sC` (ch rOB), 
  (sParen $ Sy $ unit_symb rOB)]

-- T5 : Newton's second law for rotational motion --

t5NewtonSLR_new :: TheoryModel
t5NewtonSLR_new = tm (cw newtonSLR)
  [qw QP.torque, qw QP.momentOfInertia, qw QP.angularAccel] 
  ([] :: [ConceptChunk]) [] [(sy  QP.torque) $= (sy QP.momentOfInertia) 
  * (sy QP.angularAccel)] [] [] "NewtonSecLawRotMot" [newtonSLRDesc]

newtonSLR :: RelationConcept
newtonSLR = makeRC "newtonSLR" 
  (nounPhraseSP "Newton's second law for rotational motion") newtonSLRDesc newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = (sy  QP.torque) $= (sy QP.momentOfInertia) * (sy QP.angularAccel)

-- Need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section.
newtonSLRDesc :: Sentence
newtonSLRDesc = foldlSent [S "The net", (phrase QP.torque),
  (ch QP.torque),
  (sParen $ Sy $ unit_symb  QP.torque), S "on a", (phrase CP.rigidBody),
  S "is proportional to its", (phrase QP.angularAccel),
  (ch QP.angularAccel) +:+. (sParen $ Sy $ unit_symb QP.angularAccel),
  S "Here" `sC` (ch QP.momentOfInertia),
  (sParen $ Sy $ unit_symb QP.momentOfInertia),
  S "denotes the", (phrase QP.momentOfInertia), S "of the" +:+.
  (phrase CP.rigidBody), S "We also assume that all",
  (plural CP.rigidBody), S "involved are two-dimensional",
  makeRef2S assumpOD]

  
