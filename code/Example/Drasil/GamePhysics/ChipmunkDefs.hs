{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.Drasil.GamePhysics.ChipmunkDefs where

import Example.Drasil.GamePhysics.ChipmunkUnits

import Language.Drasil
import Language.Drasil.SI_Units

import Control.Lens ((^.))

accel, angAccel, restCoef, force, gravAccel, gravConst, momtInert, impulse,
  mass, normalVect, angVel, position, orientation, dist, disp, time, torque,
  angDisp, vel :: UnitalChunk

chipSymbols :: [UnitalChunk]
chipSymbols = [accel, angAccel, restCoef, force, gravAccel, gravConst,
  momtInert, impulse, mass, normalVect, angVel, position, orientation, dist,
  disp, time, torque, angDisp, vel]

accel       = makeUC "a" "acceleration" (vec lA) accelU
angAccel    = makeUC "alpha" "angular acceleration" (Special Alpha_L) angAccelU
restCoef   = makeUC "C_R" "coefficient of restitution" (sub cC cR) unitless
force       = makeUC "F" "force" (vec cF) newton
-- how do I include numbers for constants? --
gravAccel   = makeUC "g" "gravitational acceleration" lG newton
gravConst   = makeUC "G" "gravitational constant" cG gravConstU
momtInert   = makeUC "I" "moment of inertia" (vec cI) momtInertU
impulse     = makeUC "J" "impulse" (vec cJ) impulseU
mass        = makeUC "m" "mass" lM kilogram
normalVect  = makeUC "n" "collision normal vector" (vec lN) metre
angVel      = makeUC "omega" "angular velocity" (Special Omega_L) angVelU
position    = makeUC "p" "position" (vec lP) metre
orientation = makeUC "phi" "orientation" (Special Phi_L) radians
dist        = makeUC "r" "distance" lR metre
disp        = makeUC "r" "displacement" (vec lR) metre
time        = makeUC "t" "time" lT second
torque      = makeUC "tau" "torque" (Special Tau_L) torqueU
-- theta hasn't been implemented, using gamma for now --
angDisp     = makeUC "theta" "angular displacement" (Special Gamma_L) radians
vel         = makeUC "v" "velocity" (vec lV) velU

----Acronyms-----
assumption, centreMass, dataDefn, genDefn, goalStmt, instMod, likelyChange, ode,
  requirement, srs, theoMod :: ConceptChunk

acronyms :: [ConceptChunk]
acronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt, instMod,
  likelyChange, ode, requirement, srs, theoMod]

assumption    = makeCC "A" "Assumption"
centreMass    = makeCC "CM" "Centre of Mass"
dataDefn      = makeCC "DD" "Data Definition"
genDefn       = makeCC "GD" "General Definition"
goalStmt      = makeCC "GS"  "Goal Statement"
instMod       = makeCC "IM" "Instance Model"
likelyChange  = makeCC "LC" "Likely Change"
ode           = makeCC "ODE" "Ordinary Differential Equation"
requirement   = makeCC "R" "Requirement"
srs           = makeCC "SRS" "Software Requirements Specification"
theoMod       = makeCC "T" "Theoretical Model"

-- Concept Chunks --
chipmunk, physLib, rigidBody, rigidBodies, velocity, vels, angularVel,
  angularVels, fric, elast, ctrOfMass, cartesian, rightHand :: ConceptChunk

concepts :: [ConceptChunk]
concepts = [chipmunk, physLib, rigidBody, rigidBodies, velocity, vels,
  angularVel, angularVels, fric, elast, ctrOfMass, cartesian, rightHand]

-- is there a nice way to pluralize concepts when needed? --
chipmunk    = makeCC "Chipmunk2D" "The name of this game physics library."
physLib     = makeCC "physics library" ("A programming library which " ++
  "provides functions for modelling physical phenomenon.")
rigidBody   = makeCC "rigid body" ("A solid body in which deformation is " ++
  "neglected.")
velocity    = makeCC "velocity" "The rate of change of a body's position."
angularVel  = makeCC "angular velocity" ("The rate of change of a body's " ++
  "orientation.")
fric        = makeCC "friction" ("The force resisting the relative motion " ++
  "of two surfaces.")
elast       = makeCC "elasticity" ("Ratio of the relative velocities " ++
  "of two colliding objects after and before a collision.")
ctrOfMass   = makeCC "centre of mass" ("The mean location of the " ++
  "distribution of mass of the object.")
cartesian   = makeCC "Cartesian coordinates" ("A coordinate system that " ++
  "specifies each point uniquely in a plane by a pair of numerical " ++
  "coordinates.")
rightHand   = makeCC "right-handed coordinate system" ("A coordinate " ++
  "system where the positive z-axis comes out of the screen.")

-- plural hacks (for 'irregular' plurals) --
rigidBodies = makeCC "rigid bodies" ("A solid body in which deformation is " ++
  "neglected.")
vels        = makeCC "velocities" "The rate of change of a body's position."
angularVels = makeCC "angular velocities" ("The rate of change of a body's " ++
  "orientation.")

-- Theoretical Models --

tMods :: [RelationChunk]
tMods = [t1NewtonSL, t2NewtonTL, t3NewtonLUG, t4ChaslesThm, t5NewtonSLR]

-- T1 : Newton's second law of motion --

t1NewtonSL :: RelationChunk
t1NewtonSL = makeRC "Newton's second law of motion" t1descr newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = S "The net " :+: (force ^. descr) :+: S " " :+:
  U (force ^. symbol) :+: S " (" :+: Sy (force ^. unit) :+: S ") on a " :+:
  S (rigidBody ^. name) :+: S " is proportional to the " :+:
  (accel ^. descr) :+: S " " :+: U (accel ^. symbol) :+: S " (" :+:
  Sy (accel ^. unit) :+: S ") of the " :+: S (rigidBody ^. name) :+:
  S ", where " :+: U (mass ^. symbol) :+: S " (" :+: Sy (mass ^. unit) :+:
  S ") denotes the " :+: (mass ^. descr) :+: S " of the " :+:
  S (rigidBody ^. name) :+: S " as the constant of proprotionality."

-- T2 : Newton's third law of motion --

force_1, force_2 :: UnitalChunk
force_1 = makeUC "F_1" "force exerted by the first body (on another body)"
  (sub (force ^. symbol) (Atomic "1")) newton
force_2 = makeUC "F_2" "force exerted by the second body (on another body)"
  (sub (force ^. symbol) (Atomic "2")) newton

t2NewtonTL :: RelationChunk
t2NewtonTL = makeRC "Newton's third law of motion" t2descr newtonTLRel

newtonTLRel :: Relation
newtonTLRel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = S "Every action has an equal and opposite reaction. In other " :+:
  S "words, the " :+: (force ^. descr) :+: S " " :+: U (force_1 ^. symbol) :+:
  S " (" :+: Sy (force_1 ^. unit) :+: S ") exerted on the second " :+:
  S (rigidBody ^. name) :+: S " by the first is equal in magnitude and in " :+:
  S "the opposite direction to the " :+: (force ^. descr) :+: S " " :+:
  U (force_2 ^. symbol) :+: S " (" :+: Sy (force_2 ^. unit) :+:
  S ") exerted on the first " :+: S (rigidBody ^. name) :+: S " by the second."

-- T3 : Newton's law of universal gravitation --

mass_1, mass_2, dispUnit, dispNorm, sqrDist :: UnitalChunk
mass_1  = makeUC "m_1" "mass of the first body" (sub (mass ^. symbol)
  (Atomic "1")) kilogram
mass_2  = makeUC "m_2" "mass of the second body" (sub (mass ^. symbol)
  (Atomic "2")) kilogram
dispUnit = makeUC "rhat" "unit displacement vector" (vec (hat lR)) metre
-- improvised norm symbols
dispNorm    = makeUC "||r||" "Euclidean norm of the displacement"
  (Concat [Atomic "||", (disp ^. symbol), Atomic "||"]) metre
sqrDist = makeUC "||r||^2" "squared distance" (sup (dispNorm ^. symbol)
  (Atomic "2")) m_2

t3NewtonLUG :: RelationChunk
t3NewtonLUG = makeRC "Newton's law of universal gravitation" t3descr
  newtonLUGRel

newtonLUGRel :: Relation
newtonLUGRel = (C force) :=
  (C gravConst) * ((C mass_1) * (C mass_2) / ((C dispNorm) :^ (fromInteger 2)))
  * (C dispUnit) :=
  (C gravConst) * ((C mass_1) * (C mass_2) / ((C dispNorm) :^ (fromInteger 2)))
  * ((C disp) / (C dispNorm))

-- Can't include fractions with a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184))
-- Can't include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 * 10^{-11} m/kgs^2" (line 187))
t3descr :: Sentence
t3descr = S "Two " :+: S (rigidBodies ^. name) :+: S " in the universe " :+:
  S "attract each other with a " :+: (force ^. descr) :+: S " " :+:
  U (force ^. symbol) :+: S " (" :+: Sy (force ^. unit) :+: S ") " :+:
  S "that is directly proportional to the product of their " :+:
  (mass ^. descr) :+: S "es, " :+: U (mass_1 ^. symbol) :+: S " and " :+:
  U (mass_2 ^. symbol) :+: S " (" :+: Sy (mass ^. unit) :+: S "), and " :+:
  S "inversely proportional to the " :+: (sqrDist ^. descr) :+: S " " :+:
  U (sqrDist ^. symbol) :+: S " (" :+: Sy (sqrDist ^. unit) :+: S ") " :+:
  S "between them. The vector " :+: U (disp ^. symbol) :+: S " (" :+:
  Sy (disp ^. unit) :+: S ") is the " :+: (disp ^. descr) :+: S " between " :+:
  S "the centres of the " :+: S (rigidBodies ^. name) :+: S " and " :+:
  U (dispNorm ^. symbol) :+: S " (" :+: Sy (dispNorm ^. unit) :+:
  S ") represents " :+: S "the " :+: (dispNorm ^. descr) :+:
  S ", or absolute distance between the " :+: S "two. " :+:
  U (dispUnit ^. symbol) :+: S " denotes the " :+: (dispUnit ^. descr) :+:
  S ", equivalent to the " :+: (disp ^. descr) :+: S " divided by the " :+:
  (dispNorm ^. descr) :+: S ", as shown above. " :+: S "Finally, " :+:
  U (gravConst ^. symbol) :+: S " is the " :+: (gravConst ^. descr) :+: S "."

-- T4 : Chasles' theorem --

vel_B, vel_O, r_OB :: UnitalChunk
vel_B   = makeUC "v_B" "velocity at point B" (sub (vel ^. symbol) cB) velU
vel_O   = makeUC "v_O" "velocity at the origin" (sub (vel ^. symbol) cO) velU
r_OB    = makeUC "r_OB" "displacement vector between the origin and point B"
  (sub (disp ^. symbol) (Concat [cO, cB])) metre

t4ChaslesThm :: RelationChunk
t4ChaslesThm = makeRC "Chasles' theorem" t4descr chaslesRel

-- Need the cross product symbol - third term should be a cross product.
chaslesRel :: Relation
chaslesRel = (C vel_B) := (C vel_O) + ((C angVel) * (C r_OB))

-- B should ideally be italicized in 'point B' (line 202)
t4descr :: Sentence
t4descr = S "The linear " :+: (vel ^. descr) :+: S " " :+:
  U (vel_B ^. symbol) :+: S " (" :+: Sy (vel_B ^. unit) :+: S ") of any " :+:
  S "point B in a " :+: S (rigidBody ^. name) :+: S " is the sum of the " :+:
  S "linear " :+: (vel ^. descr) :+: S " " :+: U (vel_O ^. symbol) :+:
  S " (" :+: Sy (vel_O ^. unit) :+: S ") of the " :+: S (rigidBody ^. name) :+:
  S " at the origin (axis of rotation) and the resultant vector from the " :+:
  S " cross product of the " :+: S (rigidBody ^. name) :+: S "'s " :+:
  (angVel ^. descr) :+: S " " :+: U (angVel ^. symbol) :+: S " (" :+:
  Sy (angVel ^. unit) :+: S ") and the " :+: (r_OB ^. descr) :+: S ", " :+:
  U (r_OB ^. symbol) :+: S " (" :+: Sy (r_OB ^. unit) :+: S "."

-- T5 : Newton's second law for rotational motion --

t5NewtonSLR :: RelationChunk
t5NewtonSLR = makeRC "Newton's second law for rotational motion" t5descr
  newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = (C torque) := (C momtInert) * (C angAccel)

-- need reference to A2 (line 236) -- can't reference specific assumptions
-- without referencing the entire section or dividing each bullet into its own
-- section
t5descr :: Sentence
t5descr = S "The net " :+: (torque ^. descr) :+: S " " :+:
  U (torque ^. symbol) :+: S " (" :+: Sy (torque ^. unit) :+: S ") on a " :+:
  S (rigidBody ^. name) :+: S " is proportional to its " :+:
  (angAccel ^. descr) :+: S " " :+: U (angAccel ^. symbol) :+: S " (" :+:
  Sy (angAccel ^. unit) :+: S "). Here, " :+: U (momtInert ^. symbol) :+:
  S " (" :+: Sy (momtInert ^. unit) :+: S ") denotes the " :+:
  (momtInert ^. descr) :+: S " of the " :+: S (rigidBody ^. name) :+: S ". " :+:
  S "We also assume that all " :+: S (rigidBodies ^. name) :+:
  S " involved are two-dimensional (A2)."

-- Data Definitions --

dDefs :: [EqChunk]
dDefs = [dd1CtrOfMass, dd2linDisp, dd3linVel, dd4linAcc, dd5angDisp, dd6angVel,
  dd7angAccel, dd8impulse]

-- DD1 : Centre of mass --

pos_CM, mass_i, pos_i, mTot :: UnitalChunk
pos_CM = makeUC "p_CM" ("the mass-weighted average position of a rigid " ++
  "body's particles") (sub (position ^. symbol) (Atomic "CM")) metre
mass_i = makeUC "m_i" "mass of the i-th particle" (sub (mass ^. symbol) lI)
  kilogram
pos_i = makeUC "p_i" "position vector of the i-th particle"
  (sub (position ^. symbol) lI) metre
mTot = makeUC "M" "total mass of the rigid body" cM kilogram

dd1CtrOfMass :: EqChunk
dd1CtrOfMass = fromEqn "p_CM" dd1descr (pos_CM ^. symbol) metre ctrOfMassEqn

ctrOfMassEqn :: Expr
ctrOfMassEqn = (UnaryOp (Summation (Nothing, Nothing))
  (((C mass_i) * (C pos_i)))) / (C mTot)

dd1descr :: Sentence
dd1descr = pos_CM ^. descr

-- DD2 : Linear displacement --

dd2linDisp :: EqChunk
dd2linDisp = fromEqn "r" dd2descr (Concat [(disp ^. symbol), Atomic "(",
  (time ^. symbol), Atomic ")"]) metre dispEqn

dispEqn :: Expr
dispEqn = Deriv (FCall (C position) [C time]) (C time)

dd2descr :: Sentence
dd2descr = S "the linear " :+: (disp ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its linear " :+: (position ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD3 : Linear velocity --

dd3linVel :: EqChunk
dd3linVel = fromEqn "v" dd3descr (Concat [(vel ^. symbol), Atomic "(",
  (time ^. symbol), Atomic ")"]) velU velEqn

velEqn :: Expr
velEqn = Deriv (FCall (C disp) [C time]) (C time)

dd3descr :: Sentence
dd3descr = S "the linear " :+: (vel ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its linear " :+: (vel ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD4 : Linear acceleration --

dd4linAcc :: EqChunk
dd4linAcc = fromEqn "a" dd4descr (Concat [(accel ^. symbol), Atomic "(",
  (time ^. symbol), Atomic ")"]) accelU accelEqn

accelEqn :: Expr
accelEqn = Deriv (FCall (C vel) [C time]) (C time)

dd4descr :: Sentence
dd4descr = S "the linear " :+: (accel ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its linear " :+: (accel ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD5 : Angular displacement --

dd5angDisp :: EqChunk
dd5angDisp = fromEqn "theta" dd5descr (Concat [(angDisp ^. symbol),
  Atomic "(", (time ^. symbol), Atomic ")"]) radians angDispEqn

angDispEqn :: Expr
angDispEqn = Deriv (FCall (C orientation) [C time]) (C time)

dd5descr :: Sentence
dd5descr = S "the " :+: (angDisp ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its " :+: (orientation ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD6 : Angular velocity --

dd6angVel :: EqChunk
dd6angVel = fromEqn "omega" dd6descr (Concat [(angVel ^. symbol), Atomic "(",
  (time ^. symbol), Atomic ")"]) angVelU angVelEqn

angVelEqn :: Expr
angVelEqn = Deriv (FCall (C angDisp) [C time]) (C time)

dd6descr :: Sentence
dd6descr = S "the " :+: (angVel ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its " :+: (angDisp ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD7 : Angular acceleration --

dd7angAccel :: EqChunk
dd7angAccel = fromEqn "alpha" dd7descr (Concat [(angAccel ^. symbol),
  Atomic "(", (time ^. symbol), Atomic ")"]) angAccelU angAccelEqn

angAccelEqn :: Expr
angAccelEqn = Deriv (FCall (C angVel) [C time]) (C time)

dd7descr :: Sentence
dd7descr = S "the " :+: (angAccel ^. descr) :+: S " of a " :+:
  S (rigidBody ^. name) :+: S " as a function of " :+: (time ^. descr) :+:
  S " " :+: U (time ^. symbol) :+: S " (" :+: Sy (time ^. unit) :+:
  S "), also equal to the derivative of its " :+: (angVel ^. descr) :+:
  S " with respect to " :+: (time ^. descr) :+: S " " :+: U (time ^. symbol) :+:
  S "."

-- DD8 : Impulse for collision response --

-- currently super crude, need norms and cross products --
initRelVel, mass_A, mass_B, normalLen, contDisp_A, contDisp_B, perpLen_A,
  momtInert_A, perpLen_B, momtInert_B :: UnitalChunk

initRelVel = makeUC "v_i^AB" "relative velocity between rigid bodies A and B"
  (sup (sub (vel ^. symbol) lI) (Concat [cA, cB])) velU
mass_A = makeUC "m_A" "mass of rigid body A" (sub (mass ^. symbol) cA) kilogram
mass_B = makeUC "m_B" "mass of rigid body B" (sub (mass ^. symbol) cB) kilogram
normalLen = makeUC "||n||" "length of the normal vector" (Concat [Atomic "||",
  (normalVect ^. symbol), Atomic "||"]) metre
contDisp_A = makeUC "r_AP" ("displacement vector between the centre of " ++
  "mass of rigid body A and contact point P") (sub (disp ^. symbol)
  (Concat [cA, cP])) metre
contDisp_B = makeUC "r_BP" ("displacement vector between the centre of " ++
  "mass of rigid body B and contact point P") (sub (disp ^. symbol)
  (Concat [cB, cP])) metre
perpLen_A = makeUC "||r_AP x n||" ("length of the vector perpendicular " ++
  "to the contact displacement vector of rigid body A")
  (Concat [Atomic "||", (contDisp_A ^. symbol), Atomic "*",
  (normalVect ^. symbol), Atomic "||"]) metre
perpLen_B = makeUC "||r_BP x n||" ("length of the vector perpendicular " ++
  "to the contact displacement vector of rigid body B")
  (Concat [Atomic "||", (contDisp_B ^. symbol), Atomic "*",
  (normalVect ^. symbol), Atomic "||"]) metre
momtInert_A = makeUC "I_A" "moment of inertia of rigid body A"
  (sub (momtInert ^. symbol) cA) momtInertU
momtInert_B = makeUC "I_B" "moment of inertia of rigid body B"
  (sub (momtInert ^. symbol) cB) momtInertU

dd8impulse :: EqChunk
dd8impulse = fromEqn "j" dd8descr lJ impulseU impulseEqn

-- The last two terms in the denominator should be cross products
impulseEqn :: Expr
impulseEqn = ((Neg ((fromInteger 1) + (C restCoef))) * (C initRelVel) :.
  (C normalVect)) / (((((fromInteger 1) / (C mass_A))) +
  ((fromInteger 1) / (C mass_B))) * ((C normalLen) :^ (fromInteger 2)) +
  (((C perpLen_A) :^ (fromInteger 2)) / (C momtInert_A)) +
  (((C perpLen_B) :^ (fromInteger 2))/ (C momtInert_B)))

dd8descr :: Sentence
dd8descr = S "the " :+: (impulse ^. descr) :+: S " used to determine " :+:
  S "collision response between two " :+: S (rigidBodies ^. name) :+: S "."

-- Instance Models --
