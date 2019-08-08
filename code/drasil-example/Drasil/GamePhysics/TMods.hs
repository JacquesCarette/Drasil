module Drasil.GamePhysics.TMods (tMods, newtonTL, 
newtonLUG, chaslesThm, newtonSLR) where

import Language.Drasil
import Theory.Drasil (TheoryModel, tmNoRefs)
import Utils.Drasil

import Drasil.GamePhysics.Assumptions (assumpOT, assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dispUnit, force_1, force_2,
  mass_1, mass_2, rOB, sqrDist, velB, velO)

import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (angularAccel, angularVelocity,
  displacement, force, gravitationalConst, linearVelocity, momentOfInertia,
  torque)
import Data.Drasil.Theories.Physics (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [newtonSL, newtonTL, newtonLUG, chaslesThm, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (cw newtonTL_RC) [qw force_1, qw force_2] ([] :: [ConceptChunk])
  [] [newtonTLRel] [] "NewtonThirdLawMot" [newtonTLNote]

newtonTL_RC :: RelationConcept
newtonTL_RC = makeRC "newtonTL_RC" (nounPhraseSP "Newton's third law of motion")
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
newtonLUG = tmNoRefs (cw newtonLUG_RC)
  [qw force, qw gravitationalConst, qw mass_1, qw mass_2,
  qw dispNorm, qw dispUnit, qw displacement] ([] :: [ConceptChunk])
  [] [newtonLUGRel] [] "UniversalGravLaw" newtonLUGNotes

newtonLUG_RC :: RelationConcept
newtonLUG_RC = makeRC "newtonLUG_RC" 
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
  [S "Two", plural rigidBody, S "in the universe attract each other with a",
   getTandS force, S "that is directly proportional to the product of their",
   plural mass `sC` ch mass_1 `sAnd` ch mass_2 `sC` EmptyS `sAnd`
   S "inversely proportional" `toThe` getTandS sqrDist, S "between them"],
  [ch dispUnit `sIs` S "equivalent" `toThe` phrase displacement,
   S "divided by the", phrase dispNorm `sC` S "as shown above"]]

-- T4 : Chasles' theorem --

chaslesThm :: TheoryModel
chaslesThm = tmNoRefs (cw chaslesThm_RC)
  [qw velB, qw velO, qw angularVelocity, qw rOB] 
  ([] :: [ConceptChunk]) [] [chaslesThmRel] [] "ChaslesThm" chaslesThmNotes

chaslesThm_RC :: RelationConcept
chaslesThm_RC = makeRC "chaslesThm_RC" (nounPhraseSP "Chasles' theorem")
  EmptyS chaslesThmRel

chaslesThmRel :: Relation
chaslesThmRel = sy velB $= sy velO + cross (sy angularVelocity) (sy rOB)

-- B should ideally be italicized in 'point B' (line 202).
chaslesThmNotes :: [Sentence]
chaslesThmNotes = [
  foldlSent [S "The", phrase linearVelocity, ch velB `sOf` S "any point B in a",
    phrase rigidBody `isThe` S "sum" `sOf` (phrase linearVelocity +:+ ch velO) `ofThe`
    phrase rigidBody, S "at the origin (axis of rotation)" `andThe`
    S "resultant vector from", S "cross product" `ofThe` phrasePoss rigidBody,
    getTandS angularVelocity `andThe` getTandS rOB],
  makeRef2S assumpOT]

-- T5 : Newton's second law for rotational motion --

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (cw newtonSLR_RC)
  [qw torque, qw momentOfInertia, qw angularAccel] 
  ([] :: [ConceptChunk]) [] [newtonSLRRel] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLR_RC :: RelationConcept
newtonSLR_RC = makeRC "newtonSLR_RC" 
  (nounPhraseSP "Newton's second law for rotational motion") EmptyS newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = sy torque $= sy momentOfInertia * sy angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = [
  foldlSent [S "The net", getTandS torque, S "on a", phrase rigidBody `sIs`
   S "proportional to its", getTandS angularAccel],
  makeRef2S assumpOD]
