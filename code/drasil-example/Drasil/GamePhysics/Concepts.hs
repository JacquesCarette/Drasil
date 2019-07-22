module Drasil.GamePhysics.Concepts (centreMass, threeD, twoD, gamePhysics, acronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, likelyChg,
  requirement, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.Physics (threeD, twoD)
import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, physics, thModel)
----- Acronyms -----

acronyms :: [CI]
acronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, threeD, twoD, gamePhysics,
    typUnc, unlikelyChg]

--FIXME: Should use of' combinator.
centMass :: NP --FIXME: Need to be able to cap plural.
centMass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass, gamePhysics :: CI
centreMass = commonIdeaWithDict "centreMass" centMass "CM" [physics]
gamePhysics   = commonIdeaWithDict "gamePhysics" (pn "game physics library") "Game Physics" [physics]
