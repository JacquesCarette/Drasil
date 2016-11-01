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

chipmunk, rigidBodies, vels, angularVels :: ConceptChunk

chipmunk    = makeCC "Chipmunk2D" "Chipmunk2D game physics library"

-- Chunks to accommodate 'irregular' plurals --
rigidBodies = makeCC "rigid bodies" ("A solid body in which deformation is " ++
    "neglected.")
vels        = makeCC "velocities" "The rate of change of a body's position."
angularVels = makeCC "angular velocities" ("The rate of change of a body's " ++
    "orientation.")