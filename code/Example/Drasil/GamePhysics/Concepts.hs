module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math

----- Acronyms -----

centreMass, twoD :: CI

cpAcronyms :: [CI]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

centreMass    = commonidea "centreMass" "Centre of Mass" "CM"
twoD          = commonidea "twoD" "Two-Dimensional" "2D"

chipmunk :: NamedChunk
chipmunk    = nc' "chipmunk" "Chipmunk2D game physics library" "Chipmunk2D" 
