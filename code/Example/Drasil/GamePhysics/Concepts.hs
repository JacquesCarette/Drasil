module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Prelude hiding (id)

----- Acronyms -----

centreMass, twoD :: ConceptChunk

-- FIXME: Create actual acronyms instead of using CCs.

cpAcronyms :: [ConceptChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = dcc "centreMass" "CM" "Centre of Mass"
twoD          = dcc "twoD" "2D" "Two-Dimensional"

chipmunk :: ConceptChunk

chipmunk    = dcc "chipmunk" "Chipmunk2D" "Chipmunk2D game physics library"