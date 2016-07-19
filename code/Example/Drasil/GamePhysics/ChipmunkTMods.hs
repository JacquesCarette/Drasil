{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.Drasil.GamePhysics.ChipmunkTMods where

import Example.Drasil.GamePhysics.ChipmunkUnitals
import Example.Drasil.GamePhysics.ChipmunkConcepts

import Language.Drasil

import Control.Lens ((^.))

----- Theoretical Models -----

cpTMods :: [RelationChunk]
cpTMods = [t1NewtonSL, t2NewtonTL, t3NewtonLUG, t4ChaslesThm, t5NewtonSLR]

-- T1 : Newton's second law of motion --

t1NewtonSL :: RelationChunk
t1NewtonSL = makeRC "Newton's second law of motion" t1descr newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = S "The net " :+: (force ^. descr) :+: S " " :+:
    P (force ^. symbol) :+: S " (" :+: Sy (force ^. unit) :+: S ") on a " :+:
    S (rigidBody ^. name) :+: S " is proportional to the " :+:
    (accel ^. descr) :+: S " " :+: P (accel ^. symbol) :+: S " (" :+:
    Sy (accel ^. unit) :+: S ") of the " :+: S (rigidBody ^. name) :+:
    S ", where " :+: P (mass ^. symbol) :+: S " (" :+: Sy (mass ^. unit) :+:
    S ") denotes the " :+: (mass ^. descr) :+: S " of the " :+:
    S (rigidBody ^. name) :+: S " as the constant of proprotionality."

-- T2 : Newton's third law of motion --

t2NewtonTL :: RelationChunk
t2NewtonTL = makeRC "Newton's third law of motion" t2descr newtonTLRel

newtonTLRel :: Relation
newtonTLRel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = S "Every action has an equal and opposite reaction. In other " :+:
    S "words, the " :+: (force ^. descr) :+: S " " :+: P (force_1 ^. symbol) :+:
    S " (" :+: Sy (force_1 ^. unit) :+: S ") exerted on the second " :+:
    S (rigidBody ^. name) :+: S " by the first is equal in magnitude and " :+:
    S "in the opposite direction to the " :+: (force ^. descr) :+: S " " :+:
    P (force_2 ^. symbol) :+: S " (" :+: Sy (force_2 ^. unit) :+:
    S ") exerted on the first " :+: S (rigidBody ^. name) :+:
    S " by the second."

-- T3 : Newton's law of universal gravitation --

t3NewtonLUG :: RelationChunk
t3NewtonLUG = makeRC "Newton's law of universal gravitation" t3descr
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
t3descr = S "Two " :+: S (rigidBodies ^. name) :+: S " in the universe " :+:
    S "attract each other with a " :+: (force ^. descr) :+: S " " :+:
    P (force ^. symbol) :+: S " (" :+: Sy (force ^. unit) :+: S ") " :+:
    S "that is directly proportional to the product of their " :+:
    (mass ^. descr) :+: S "es, " :+: P (mass_1 ^. symbol) :+: S " and " :+:
    P (mass_2 ^. symbol) :+: S " (" :+: Sy (mass ^. unit) :+: S "), and " :+:
    S "inversely proportional to the " :+: (sqrDist ^. descr) :+: S " " :+:
    P (sqrDist ^. symbol) :+: S " (" :+: Sy (sqrDist ^. unit) :+: S ") " :+:
    S "between them. The vector " :+: P (disp ^. symbol) :+: S " (" :+:
    Sy (disp ^. unit) :+: S ") is the " :+: (disp ^. descr) :+:
    S " between the centres of the " :+: S (rigidBodies ^. name) :+:
    S " and " :+: P (dispNorm ^. symbol) :+: S " (" :+:
    Sy (dispNorm ^. unit) :+: S ") represents " :+: S "the " :+:
    (dispNorm ^. descr) :+: S ", or absolute distance between the " :+:
    S "two. " :+: P (dispUnit ^. symbol) :+: S " denotes the " :+:
    (dispUnit ^. descr) :+: S ", equivalent to the " :+: (disp ^. descr) :+:
    S " divided by the " :+: (dispNorm ^. descr) :+: S ", as shown above. " :+:
    S "Finally, " :+: P (gravConst ^. symbol) :+: S " is the " :+:
    (gravConst ^. descr) :+: S " ( " :+: Sy (gravConst ^. unit) :+: S ")."

-- T4 : Chasles' theorem --

t4ChaslesThm :: RelationChunk
t4ChaslesThm = makeRC "Chasles' theorem" t4descr chaslesRel

-- Need the cross product symbol - third term should be a cross product.
chaslesRel :: Relation
chaslesRel = (C vel_B) := (C vel_O) + ((C angVel) * (C r_OB))

-- B should ideally be italicized in 'point B' (line 202).
t4descr :: Sentence
t4descr = S "The linear " :+: (vel ^. descr) :+: S " " :+:
    P (vel_B ^. symbol) :+: S " (" :+: Sy (vel_B ^. unit) :+: S ") of any " :+:
    S "point B in a " :+: S (rigidBody ^. name) :+: S " is the sum of the " :+:
    S "linear " :+: (vel ^. descr) :+: S " " :+: P (vel_O ^. symbol) :+:
    S " (" :+: Sy (vel_O ^. unit) :+: S ") of the " :+:
    S (rigidBody ^. name) :+: S " at the origin (axis of rotation) and the " :+:
    S "resultant vector from the cross product of the " :+:
    S (rigidBody ^. name) :+: S "'s " :+: (angVel ^. descr) :+: S " " :+:
    P (angVel ^. symbol) :+: S " (" :+: Sy (angVel ^. unit) :+:
    S ") and the " :+: (r_OB ^. descr) :+: S ", " :+: P (r_OB ^. symbol) :+:
    S " (" :+: Sy (r_OB ^. unit) :+: S ")."

-- T5 : Newton's second law for rotational motion --

t5NewtonSLR :: RelationChunk
t5NewtonSLR = makeRC "Newton's second law for rotational motion" t5descr
  newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = (C torque) := (C momtInert) * (C angAccel)

-- Need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section.
t5descr :: Sentence
t5descr = S "The net " :+: (torque ^. descr) :+: S " " :+:
    P (torque ^. symbol) :+: S " (" :+: Sy (torque ^. unit) :+: S ") on a " :+:
    S (rigidBody ^. name) :+: S " is proportional to its " :+:
    (angAccel ^. descr) :+: S " " :+: P (angAccel ^. symbol) :+: S " (" :+:
    Sy (angAccel ^. unit) :+: S "). Here, " :+: P (momtInert ^. symbol) :+:
    S " (" :+: Sy (momtInert ^. unit) :+: S ") denotes the " :+:
    (momtInert ^. descr) :+: S " of the " :+: S (rigidBody ^. name) :+:
    S ". We also assume that all " :+: S (rigidBodies ^. name) :+:
    S " involved are two-dimensional (A2)."
