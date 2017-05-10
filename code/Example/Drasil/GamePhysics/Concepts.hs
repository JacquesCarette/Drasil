module Drasil.GamePhysics.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math

----- Acronyms -----

centreMass, twoD :: CINP

cpAcronyms :: [CINP]
cpAcronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt,
    inModel, likelyChg, ode, requirement, srs, thModel, twoD]

--FIXME: Should use of' combinator.
cent_mass :: NP --FIXME: Need to be able to cap plural.
cent_mass = nounPhrase' "centre of mass" "centres of mass" 
  (Replace (S "centre of mass"))

centreMass    = commonINP "centreMass" cent_mass              "CM"
twoD          = commonINP "twoD"       (pn "Two-Dimensional") "2D"

chipmunk :: NPNC
chipmunk    = npnc' "chipmunk" (pn "Chipmunk2D game physics library") "Chipmunk2D" 
