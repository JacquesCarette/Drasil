module Drasil.GamePhysics.Concepts (centreMass, twoD, chipmunk, acronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, goalStmt, likelyChg,
  requirement, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.IdeaDicts (dataDefn, genDefn, inModel, physics, thModel)
----- Acronyms -----

centreMass, chipmunk :: CI

acronyms :: [CI]
acronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD, chipmunk,
    typUnc, unlikelyChg]

--FIXME: Should use of' combinator.
centMass :: NP --FIXME: Need to be able to cap plural.
centMass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass    = commonIdeaWithDict "centreMass" centMass              "CM"   [physics]

chipmunk = commonIdeaWithDict "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D"  [physics]
