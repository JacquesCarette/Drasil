module Drasil.GamePhysics.TMods where

import Drasil.GamePhysics.Unitals

import Language.Drasil
import Data.Drasil.Concepts.Physics (rigidBody)
import Prelude hiding (id)
import Control.Lens ((^.))

----- Theoretical Models -----

cpTMods :: [RelationConcept]
cpTMods = [t1NewtonSL, t2NewtonTL, t3NewtonLUG, t4ChaslesThm, t5NewtonSLR]

-- T1 : Newton's second law of motion --

t1NewtonSL :: RelationConcept
t1NewtonSL = makeRC "t1NewtonSL" "Newton's second law of motion" 
  t1descr newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = S "The net" +:+ (force ^. term) +:+ P (force ^. symbol) +:+ 
  sParen (Sy (force ^. unit)) +:+ S "on a" +:+ (rigidBody ^. term) +:+ 
  S "is proportional to the" +:+ (accel ^. term) +:+ P (accel ^. symbol) +:+ 
  sParen (Sy (accel ^. unit)) +:+ S "of the" +:+ (rigidBody ^. term) `sC`
  S "where" +:+ P (mass ^. symbol) +:+ sParen (Sy (mass ^. unit)) +:+
  S "denotes the" +:+ (mass ^. term) +:+ S "of the" +:+ (rigidBody ^. term) +:+.
  S "as the constant of proprotionality"

-- T2 : Newton's third law of motion --

t2NewtonTL :: RelationConcept
t2NewtonTL = makeRC "t2NewtonTL" "Newton's third law of motion" t2descr newtonTLRel

newtonTLRel :: Relation
newtonTLRel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = S "Every action has an equal and opposite reaction. In other" +:+
  S "words, the" +:+ (force ^. term) +:+ P (force_1 ^. symbol) +:+
  sParen (Sy (force_1 ^. unit)) +:+ S "exerted on the second" +:+
  (rigidBody ^. term) +:+ S "by the first is equal in magnitude and" +:+
  S "in the opposite direction to the" +:+ (force ^. term) +:+
  P (force_2 ^. symbol) +:+ sParen (Sy (force_2 ^. unit)) +:+
  S "exerted on the first" +:+ (rigidBody ^. term) +:+. S "by the second"

-- T3 : Newton's law of universal gravitation --

t3NewtonLUG :: RelationConcept
t3NewtonLUG = makeRC "t3NewtonLUG" "Newton's law of universal gravitation" t3descr
  newtonLUGRel

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
t3descr = S "Two" +:+ irregPlur (rigidBody ^. term) +:+ S "in the universe" +:+
  S "attract each other with a" +:+ (force ^. term) +:+ 
  P (force ^. symbol) +:+ sParen (Sy (force ^. unit)) +:+
  S "that is directly proportional to the product of their" +:+
  (mass ^. term) :+: S "es" `sC` P (mass_1 ^. symbol) +:+ S "and" +:+
  P (mass_2 ^. symbol) +:+ sParen (Sy (mass ^. unit)) `sC` S "and" +:+
  S "inversely proportional to the" +:+ (sqrDist ^. term) +:+
  P (sqrDist ^. symbol) +:+ sParen (Sy (sqrDist ^. unit)) +:+
  S "between them. The vector" +:+ P (disp ^. symbol) +:+ 
  sParen (Sy (disp ^. unit)) +:+ S "is the" +:+ (disp ^. term) +:+
  S "between the centres of the" +:+ irregPlur (rigidBody ^. term) +:+
  S "and" +:+ P (dispNorm ^. symbol) +:+ sParen (Sy (dispNorm ^. unit)) +:+ 
  S "represents the" +:+. ((dispNorm ^. term) `sC`
  S "or absolute distance between the two") +:+ P (dispUnit ^. symbol) +:+ 
  S "denotes the" +:+ (dispUnit ^. term) `sC` S "equivalent to the" +:+
  (disp ^. term) +:+ S "divided by the" +:+. ((dispNorm ^. term) `sC`
  S "as shown above") +:+ S "Finally" `sC` P (gravConst ^. symbol) +:+ 
  S "is the" +:+ (gravConst ^. defn) +:+. sParen (Sy (gravConst ^. unit))

-- T4 : Chasles' theorem --

t4ChaslesThm :: RelationConcept
t4ChaslesThm = makeRC "t4ChaslesThm" "Chasles' theorem" t4descr chaslesRel

-- Need the cross product symbol - third term should be a cross product.
chaslesRel :: Relation
chaslesRel = (C vel_B) := (C vel_O) + ((C angVel) * (C r_OB))

-- B should ideally be italicized in 'point B' (line 202).
t4descr :: Sentence
t4descr = S "The linear" +:+ (vel ^. term) +:+ P (vel_B ^. symbol) +:+ 
  sParen (Sy (vel_B ^. unit)) +:+ S "of any point B in a" +:+ 
  (rigidBody ^. term) +:+ S "is the sum of the linear" +:+ (vel ^. term) +:+ 
  P (vel_O ^. symbol) +:+ sParen (Sy (vel_O ^. unit)) +:+ S "of the" +:+
  (rigidBody ^. term) +:+ S "at the origin (axis of rotation) and the" +:+
  S "resultant vector from the cross product of the" +:+
  (rigidBody ^. term) :+: S "'s" +:+ ((angVel ^. term)) +:+ 
  P (angVel ^. symbol) +:+ sParen (Sy (angVel ^. unit)) +:+ S "and the" +:+ 
  (r_OB ^. term) `sC` P (r_OB ^. symbol) +:+. sParen (Sy (r_OB ^. unit))

-- T5 : Newton's second law for rotational motion --

t5NewtonSLR :: RelationConcept
t5NewtonSLR = makeRC "t5NewtonSLR" "Newton's second law for rotational motion" 
  t5descr newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = (C torque) := (C momtInert) * (C angAccel)

-- Need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section.
t5descr :: Sentence
t5descr = S "The net" +:+ (torque ^. term) +:+ P (torque ^. symbol) +:+ 
  sParen (Sy (torque ^. unit)) +:+ S "on a" +:+ (rigidBody ^. term) +:+ 
  S "is proportional to its" +:+ (angAccel ^. term) +:+ P (angAccel ^. symbol) +:+.
  sParen (Sy (angAccel ^. unit)) +:+ S "Here" `sC` P (momtInert ^. symbol) +:+
  sParen (Sy (momtInert ^. unit)) +:+ S "denotes the" +:+ (momtInert ^. term) +:+
  S "of the" +:+. (rigidBody ^. term) +:+ S "We also assume that all" +:+ 
  irregPlur (rigidBody ^. term) +:+. S "involved are two-dimensional (A2)"
