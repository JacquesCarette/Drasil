module Drasil.GamePhysics.Concepts (centreMass, twoD, chipmunk, cpAcronyms) where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math

----- Acronyms -----

centreMass, twoD :: CI

cpAcronyms :: [CI]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

--FIXME: Should use of' combinator.
cent_mass :: NP --FIXME: Need to be able to cap plural.
cent_mass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass    = commonIdea "centreMass" cent_mass              "CM"
twoD          = commonIdea "twoD"       (pn "Two-Dimensional") "2D"

chipmunk :: NamedChunk
chipmunk      = npnc' "chipmunk"      (pn "Chipmunk2D game physics library")    "Chipmunk2D"