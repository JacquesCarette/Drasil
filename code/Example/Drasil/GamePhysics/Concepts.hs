module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Prelude hiding (id)

----- Acronyms -----

centreMass, twoD :: NamedChunk

-- FIXME: Create actual acronyms instead of using CCs.

cpAcronyms :: [NamedChunk]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = nc' "centreMass" "Centre of Mass" "CM"
twoD          = nc' "twoD" "Two-Dimensional" "2D"

chipmunk :: ConceptChunk

chipmunk    = dcc "chipmunk" "Chipmunk2D" "Chipmunk2D game physics library"