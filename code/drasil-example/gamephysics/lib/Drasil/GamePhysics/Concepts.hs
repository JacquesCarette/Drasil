module Drasil.GamePhysics.Concepts (centreMass, threeD, twoD, gamePhysics, acronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, likelyChg,
  requirement, refBy, refName, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Physics (threeD, twoD)
import Data.Drasil.TheoryConcepts (dataDefn, genDefn, inModel, thModel)
import Data.Drasil.Domains (physics)

import Control.Lens ((^.))

----- Acronyms -----

acronyms :: [CI]
acronyms = [assumption, centreMass, dataDefn, gamePhysics, genDefn, goalStmt, inModel,
  likelyChg, ode, requirement, refBy, refName, srs, thModel, threeD, twoD, typUnc, unlikelyChg]

centreMass, gamePhysics :: CI
centreMass  = commonIdeaWithDict "centreMass"  (ctrOfMass ^. term) "CM" [physics]
gamePhysics = commonIdeaWithDict "gamePhysics" (pn "game physics library") "Game Physics" [physics]
