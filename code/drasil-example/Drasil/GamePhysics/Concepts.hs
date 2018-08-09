module Drasil.GamePhysics.Concepts (centreMass, twoD, chipmunk, cpAcronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (assumption, dataDefn, genDefn, 
    goalStmt, inModel, likelyChg, requirement, srs, thModel, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)

----- Acronyms -----

centreMass, twoD, chipmunk :: CI

cpAcronyms :: [CI]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD, chipmunk,
    typUnc, unlikelyChg]

--FIXME: Should use of' combinator.
cent_mass :: NP --FIXME: Need to be able to cap plural.
cent_mass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass    = commonIdea "centreMass" cent_mass              "CM"
twoD          = commonIdea "twoD"       (pn "Two-Dimensional") "2D"

chipmunk = commonIdea "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D"
