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
ctrOfMass   = makeCC "center of mass" ("The mean location of the " ++
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

{-
--Theoretical models--

--Newton's Second Law--

t1newtonSndLaw :: RelationChunk
t1newtonSndLaw = makeRC "Newton's second law of motion" t1descr snd_law_rel

snd_law_rel :: Relation
snd_law_rel = (C force) := (C mass) * (C accel)

t1descr :: Sentence
t1descr = S "The net force on an object is proportional to the object's " :+:
  S "acceleration."

--Newton's Third Law--

t2newtonThdLaw :: RelationChunk
t2newtonThdLaw = makeRC "Newton's third law of motion" t2descr thd_law_rel

thd_law_rel :: Relation
thd_law_rel = (C force_1) := (Neg (C force_2))

t2descr :: Sentence
t2descr = S "Every action has an equal and opposite reaction."

-- Newton's Law of Universal Gravitation --

t3newtonUnivGravLaw :: RelationChunk
t3newtonUnivGravLaw = makeRC "Newton's law of universal gravitation" t3descr grav_law_rel

grav_law_rel :: Relation
grav_law_rel = (C force) := (C gravConst) *
               (((C mass_1) * (C mass_2) * (C centreDisp)) /
                ((C sqrDist) * (C centreDist)))

t3descr :: Sentence
t3descr = S "Any two bodies in the universe attract each other with a " :+:
  S "force that is directly proportional to the product of their masses " :+:
  S "and inversely proportional to the square of the distance between them."

-- Hooke's Law --

t4hookeLaw :: RelationChunk
t4hookeLaw = makeRC "Hooke's law" t4descr hooke_law_rel

hooke_law_rel :: Relation
hooke_law_rel = (C force) := (C springConst) * (C springDisp)

t4descr :: Sentence
t4descr = S "The force needed to extend or compress a spring by some " :+:
  S "distance is proportional to that distance."

-- Equation for Rotational Motion --

t5rotMotionEq :: RelationChunk
t5rotMotionEq = makeRC "Equation for rotational motion" t5descr rotation_eq_rel

rotation_eq_rel :: Relation
rotation_eq_rel = (C torque) := (C momtInert) * (C angAccel)

t5descr :: Sentence
t5descr = S "The net torque on an object is proportional to its angular " :+:
  S "acceleration."
-}

{-
PCM Example kept for reference:

t1consThermE :: RelationChunk
t1consThermE = makeRC "Conservation of thermal energy" t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) :=
  (C density) * (C htCap) * (Deriv (C temp) (C time))

t1descr :: Sentence
t1descr =
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+:
  (U $ htCap ^. symbol) :+: S " and density " :+: (U $ density ^. symbol) :+:
  S ", where " :+: (U $ thFluxVect ^. symbol))
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.
-}
