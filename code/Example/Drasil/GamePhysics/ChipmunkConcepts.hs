{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module Example.Drasil.GamePhysics.ChipmunkConcepts where

import Language.Drasil

----- Acronyms -----

assumption, centreMass, dataDefn, genDefn, goalStmt, instMod, likelyChange, ode,
    requirement, srs, theoMod, twoD :: ConceptChunk

chipmunkAcronyms :: [ConceptChunk]
chipmunkAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    instMod, likelyChange, ode, requirement, srs, theoMod, twoD]

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
twoD          = makeCC "2D" "Two-Dimensional"

----- General concepts -----

chipmunk, physLib, rigidBody, rigidBodies, velocity, vels, angularVel,
    angularVels, fric, elast, coll, space, ctrOfMass, cartesian,
    rightHand :: ConceptChunk

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
