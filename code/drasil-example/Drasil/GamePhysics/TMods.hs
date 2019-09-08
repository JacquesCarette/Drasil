module Drasil.GamePhysics.TMods (tMods, newtonSL, newtonSLR) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dispUnit, force_1, force_2,
  mass_1, mass_2, sqrDist)

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Math (vector)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (angularAccel, displacement, distance,
  force, gravitationalConst, momentOfInertia, torque)
import Data.Drasil.Theories.Physics (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [newtonSL, newtonTL, newtonLUG, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (cw newtonTLRC) [qw force_1, qw force_2]
  ([] :: [ConceptChunk]) [] [newtonTLRel] [] "NewtonThirdLawMot" [newtonTLNote]

newtonTLRC :: RelationConcept
newtonTLRC = makeRC "newtonTLRC" (nounPhraseSP "Newton's third law of motion")
  EmptyS newtonTLRel

newtonTLRel :: Relation
newtonTLRel = sy force_1 $= negate (sy force_2)

newtonTLNote :: Sentence
newtonTLNote = foldlSent [S "Every action has an equal and opposite reaction.",
  S "In other words, the", phrase force, ch force_1, S "exerted on the second",
  phrase rigidBody, S "by the first is equal in magnitude and in the opposite direction" `toThe`
  phrase force, ch force_2, S "exerted on the first", phrase rigidBody, S "by the second"]

-- T3 : Newton's law of universal gravitation --

newtonLUG :: TheoryModel
newtonLUG = tmNoRefs (cw newtonLUGRC)
  [qw force, qw gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dispUnit, qw displacement] ([] :: [ConceptChunk])
  [] [newtonLUGRel] [] "UniversalGravLaw" newtonLUGNotes

newtonLUGRC :: RelationConcept
newtonLUGRC = makeRC "newtonLUGRC" 
  (nounPhraseSP "Newton's law of universal gravitation") EmptyS newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = sy force $=
  sy gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * sy dispUnit $=
  sy gravitationalConst * (sy mass_1 * sy mass_2 /
  (sy dispNorm $^ 2)) * (sy displacement / sy dispNorm)

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187)).

newtonLUGNotes :: [Sentence]
newtonLUGNotes = map foldlSent [
  [S "Two", plural rigidBody `inThe` S "universe attract each other with a",
   getTandS force, S "that is directly proportional to the product of their",
   plural mass `sC` ch mass_1 `sAnd` ch mass_2 `sC` EmptyS `sAnd`
   S "inversely proportional" `toThe` getTandS sqrDist, S "between them"],
  [S "The", phrase vector, ch displacement `isThe` phrase displacement,
   S "between", S "centres" `ofThe` plural rigidBody `sAnd` ch dispNorm `isThe`
   S "absolute", phrase distance, S "between the two"],
  [ch dispUnit `sIs` S "equivalent" `toThe` phrase displacement,
   S "divided by the", phrase dispNorm `sC` S "as shown above"]]

-- T4 : Newton's second law for rotational motion --

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
  [S "The net", getTandS torque, S "on a", phrase rigidBody `sIs`
   S "proportional to its", getTandS angularAccel `sC` S "where",
   ch momentOfInertia, S "denotes", phrase momentOfInertia `ofThe`
   phrase rigidBody, S "as the", phrase constant `sOf` S "proportionality"],
  [S "We also assume that all", plural rigidBody, S "involved" `sAre`
   phrase twoD, fromSource assumpOD]]
