module Drasil.GamePhysics.TMods where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import Data.Drasil.Utils (foldlSent)
import Data.Drasil.Concepts.Physics (rigidBody)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Prelude hiding (id)
import Control.Lens ((^.))

----- Theoretical Models -----

cpTMods :: [RelationConcept]
cpTMods = [t1NewtonSL, t2NewtonTL, t3NewtonLUG, t4ChaslesThm, t5NewtonSLR]

-- T1 : Newton's second law of motion --

t1NewtonSL :: RelationConcept
t1NewtonSL = makeRC "t1NewtonSL" (nounPhraseSP "Newton's second law of motion")
  t1descr newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = foldlSent [S "The net", (phrase $ force ^. term), (P $ force ^. symbol), 
  (sParen $ Sy $ unit_symb force), S "on a", (phrase $ rigidBody ^. term),
  S "is proportional to the", (phrase $ accel ^. term), (P $ accel ^. symbol), 
  (sParen $ Sy $ unit_symb accel), S "of the", (phrase $ rigidBody ^. term) `sC`
  S "where", (P $ mass ^. symbol), (sParen $ Sy $ unit_symb mass),
  S "denotes the", (phrase $ mass ^. term), S "of the", (phrase $ rigidBody ^. term),
  S "as the constant of proprotionality"]

-- T2 : Newton's third law of motion --

t2NewtonTL :: RelationConcept
t2NewtonTL = makeRC "t2NewtonTL" (nounPhraseSP "Newton's third law of motion")
  t2descr newtonTLRel

newtonTLRel :: Relation
newtonTLRel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = foldlSent [S "Every action has an equal and opposite reaction. In other",
  S "words, the", (phrase $ force ^. term), (P $ force_1 ^. symbol),
  (sParen $ Sy $ unit_symb force_1), S "exerted on the second",
  (phrase $ rigidBody ^. term), S "by the first is equal in magnitude and",
  S "in the opposite direction to the", (phrase $ force ^. term),
  (P $ force_2 ^. symbol), (sParen $ Sy $ unit_symb force_2),
  S "exerted on the first", (phrase $ rigidBody ^. term), S "by the second"]

-- T3 : Newton's law of universal gravitation --

t3NewtonLUG :: RelationConcept
t3NewtonLUG = makeRC "t3NewtonLUG" 
  (nounPhraseSP "Newton's law of universal gravitation") t3descr newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = (C force) :=
  (C gravConst) * ((C mass_1) * (C mass_2) /
  ((C dispNorm) :^ (fromInteger 2))) * (C dispUnit) :=
  (C gravConst) * ((C mass_1) * (C mass_2) /
  ((C dispNorm) :^ (fromInteger 2))) * ((C disp) / (C dispNorm))

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187)).

t3descr :: Sentence
t3descr = foldlSent [S "Two", (plural $ rigidBody ^. term), S "in the universe",
  S "attract each other with a", (phrase $ force ^. term), 
  (P $ force ^. symbol), (sParen $ Sy $ unit_symb force),
  S "that is directly proportional to the product of their",
  (plural $ mass ^. term) `sC` (P $ mass_1 ^. symbol), S "and",
  (P $ mass_2 ^. symbol), (sParen $ Sy $ unit_symb mass) `sC` S "and",
  S "inversely proportional to the", (phrase $ sqrDist ^. term),
  (P $ sqrDist ^. symbol), (sParen $ Sy $ unit_symb sqrDist),
  S "between them. The vector", (P $ disp ^. symbol), 
  (sParen $ Sy $ unit_symb disp), S "is the", (phrase $ disp ^. term),
  S "between the centres of the", (plural $ rigidBody ^. term),
  S "and", (P $ dispNorm ^. symbol), (sParen $ Sy $ unit_symb dispNorm), 
  S "represents the" +:+. ((phrase $ dispNorm ^. term) `sC`
  S "or absolute distance between the two"), (P $ dispUnit ^. symbol), 
  S "denotes the", (phrase $ dispUnit ^. term) `sC` S "equivalent to the",
  (phrase $ disp ^. term), S "divided by the" +:+. ((phrase $ dispNorm ^. term) `sC`
  S "as shown above"), S "Finally" `sC` (P $ gravConst ^. symbol), 
  S "is the", (gravConst ^. defn), (sParen $ Sy $ unit_symb gravConst)]

-- T4 : Chasles' theorem --

t4ChaslesThm :: RelationConcept
t4ChaslesThm = makeRC "t4ChaslesThm" (nounPhraseSP "Chasles' theorem")
  t4descr chaslesRel

-- Need the cross product symbol - third term should be a cross product.
chaslesRel :: Relation
chaslesRel = (C vel_B) := (C vel_O) + ((C angVel) * (C r_OB))

-- B should ideally be italicized in 'point B' (line 202).
t4descr :: Sentence
t4descr = foldlSent [S "The linear", (phrase $ vel ^. term), P (vel_B ^. symbol), 
  (sParen $ Sy $ unit_symb vel_B), S "of any point B in a",
  (phrase $ rigidBody ^. term), S "is the sum of the linear", 
  (phrase $ vel ^. term), (P $ vel_O ^. symbol), (sParen $ Sy $ unit_symb vel_O),
  S "of the", (phrase $ rigidBody ^. term),
  S "at the origin (axis of rotation) and the",
  S "resultant vector from the cross product of the",
  (phrase $ rigidBody ^. term) :+: S "'s", (phrase $ angVel ^. term), 
  (P $ angVel ^. symbol), (sParen $ Sy $ unit_symb angVel), S "and the", 
  (phrase $ r_OB ^. term) `sC` (P $ r_OB ^. symbol), 
  (sParen $ Sy $ unit_symb r_OB)]

-- T5 : Newton's second law for rotational motion --

t5NewtonSLR :: RelationConcept
t5NewtonSLR = makeRC "t5NewtonSLR" 
  (nounPhraseSP "Newton's second law for rotational motion") t5descr newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = (C torque) := (C momtInert) * (C angAccel)

-- Need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section.
t5descr :: Sentence
t5descr = S "The net" +:+ (phrase $ torque ^. term) +:+ P (torque ^. symbol) +:+ 
  sParen (Sy (unit_symb torque)) +:+ S "on a" +:+ (phrase $ rigidBody ^. term) +:+ 
  S "is proportional to its" +:+ (phrase $ angAccel ^. term) +:+ P (angAccel ^. symbol) +:+.
  sParen (Sy (unit_symb angAccel)) +:+ S "Here" `sC` P (momtInert ^. symbol) +:+
  sParen (Sy (unit_symb momtInert)) +:+ S "denotes the" +:+ (phrase $ momtInert ^. term) +:+
  S "of the" +:+. (phrase $ rigidBody ^. term) +:+ S "We also assume that all" +:+ 
  (plural $ rigidBody ^. term) +:+. S "involved are two-dimensional (A2)"
