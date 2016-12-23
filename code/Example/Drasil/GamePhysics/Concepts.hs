module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.Physics

import Control.Lens ((^.))

----- Acronyms -----

centreMass, twoD :: ConceptChunk

-- FIXME: Create actual acronyms instead of using CCs.

cpAcronyms :: [ConceptChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = dcc "centreMass" "CM" "Centre of Mass"
twoD          = dcc "twoD" "2D" "Two-Dimensional"

chipmunk, rigidBodies, vels, angularVels :: NamedChunk

chipmunk    = makeCC "Chipmunk2D" "Chipmunk2D game physics library"

-- Chunks to accommodate 'irregular' plurals --
-- FIXME: Figure out a way around creating duplicate chunks for irregular
--    plurals.
rigidBodies = CC "rigid bodies" (rigidBody ^. term)
vels        = CC "velocities" (velocity ^. term)
angularVels = CC "angular velocities" (angularV ^. term)