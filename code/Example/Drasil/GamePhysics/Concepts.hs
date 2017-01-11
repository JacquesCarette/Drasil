module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.Physics
import Prelude hiding (id)

import Control.Lens ((^.))

----- Acronyms -----

centreMass, twoD :: ConceptChunk

-- FIXME: Create actual acronyms instead of using CCs.

cpAcronyms :: [ConceptChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = dcc "centreMass" "CM" "Centre of Mass"
twoD          = dcc "twoD" "2D" "Two-Dimensional"

chipmunk, rigidBodies, vels, angularVels :: ConceptChunk

chipmunk    = dcc "chipmunk" "Chipmunk2D" "Chipmunk2D game physics library"

-- Chunks to accommodate 'irregular' plurals --
-- FIXME: Figure out a way around creating duplicate chunks for irregular
--    plurals.
rigidBodies = dccWDS (rigidBody ^. id) "rigid bodies" (rigidBody ^. defn)
vels        = dccWDS (velocity ^. id) "velocities" (velocity ^. defn)
angularVels = dccWDS (angularV ^. id) "angular velocities" (angularV ^. defn)
-- Hack for irregularly plural chunks -> They're the same except for the term