module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.Physics

import Control.Lens ((^.))

----- Acronyms -----

centreMass, twoD :: ConceptChunk

cpAcronyms :: [ConceptChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = makeCC "CM" "Centre of Mass"
twoD          = makeCC "2D" "Two-Dimensional"

chipmunk, rigidBodies, vels, angularVels :: ConceptChunk

chipmunk    = makeCC "Chipmunk2D" "Chipmunk2D game physics library"

-- Chunks to accommodate 'irregular' plurals --
-- FIXME: Figure out a way around creating duplicate chunks for irregular
--    plurals.
rigidBodies = CC "rigid bodies" (rigidBody ^. descr)
vels        = CC "velocities" (velocity ^. descr)
angularVels = CC "angular velocities" (angularV ^. descr)