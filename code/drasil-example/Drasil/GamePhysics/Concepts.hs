module Drasil.GamePhysics.Concepts (centreMass, twoD, chipmunk, cpAcronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, dataDefn, genDefn, 
    goalStmt, inModel, likelyChg, requirement, srs, thModel, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.Physics (twoD)
import Data.Drasil.IdeaDicts (physics)
----- Acronyms -----

centreMass, chipmunk :: CI

cpAcronyms :: [CI]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD, chipmunk,
    typUnc, unlikelyChg]

--FIXME: Should use of' combinator.
cent_mass :: NP --FIXME: Need to be able to cap plural.
cent_mass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass    = commonIdeaWithDict "centreMass" cent_mass              "CM"   [physics]

chipmunk = commonIdeaWithDict "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D"  [physics]
