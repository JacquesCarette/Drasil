module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation

----- Acronyms -----

centreMass, ode, twoD :: ConceptChunk

cpAcronyms :: [ConceptChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = makeCC "CM" "Centre of Mass"
ode           = makeCC "ODE" "Ordinary Differential Equation"
twoD          = makeCC "2D" "Two-Dimensional"

----- Physics concepts -----

chipmunk, rigidBody, rigidBodies, velocity, vels, angularVel,
    angularVels, fric, elast, coll, space, ctrOfMass, cartesian,
    rightHand :: ConceptChunk

chipmunk    = makeCC "Chipmunk2D" "Chipmunk2D game physics library"

rigidBody   = makeCC "rigid body" ("A solid body in which deformation is " ++
    "neglected.")
velocity    = makeCC "velocity" "The rate of change of a body's position."
angularVel  = makeCC "angular velocity" ("The rate of change of a body's " ++
    "orientation.")
fric        = makeCC "friction" ("The force resisting the relative motion " ++
    "of two surfaces.")
elast       = makeCC "elasticity" ("Ratio of the relative velocities " ++
    "of two colliding objects after and before a collision.")
coll        = makeCC "collision" ("An encounter between particles resulting " ++
    "in an exchange or transformation of energy.")
space       = makeCC "space" ("A two-dimensional extent where objects and " ++
    "events have relative positions and directions.")
ctrOfMass   = makeCC "centre of mass" ("The mean location of the " ++
    "distribution of mass of the object.")
cartesian   = makeCC "Cartesian coordinates" ("A coordinate system that " ++
    "specifies each point uniquely in a plane by a pair of numerical " ++
    "coordinates.")
rightHand   = makeCC "right-handed coordinate system" ("A coordinate " ++
    "system where the positive z-axis comes out of the screen.")
-- Chunks to accommodate 'irregular' plurals --
rigidBodies = makeCC "rigid bodies" ("A solid body in which deformation is " ++
    "neglected.")
vels        = makeCC "velocities" "The rate of change of a body's position."
angularVels = makeCC "angular velocities" ("The rate of change of a body's " ++
    "orientation.")
