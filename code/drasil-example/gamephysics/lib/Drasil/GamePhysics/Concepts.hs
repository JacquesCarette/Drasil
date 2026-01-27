module Drasil.GamePhysics.Concepts (centreMass, threeD, twoD, acronyms) where

import Language.Drasil

import Drasil.Metadata.Domains (physics)
import Drasil.Metadata.TheoryConcepts (dataDefn, genDefn, inModel, thModel)

import Data.Drasil.Concepts.Documentation (assumption, goalStmt, likelyChg,
  refBy, refName, requirement, srs, typUnc, unlikelyChg)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Physics (threeD, twoD)

import Control.Lens ((^.))

----- Acronyms -----

acronyms :: [CI]
acronyms = [assumption, centreMass, dataDefn, genDefn, goalStmt, inModel,
  likelyChg, ode, requirement, refBy, refName, srs, thModel, threeD, twoD, typUnc, unlikelyChg]

centreMass :: CI
centreMass  = commonIdeaWithDict "centreMass"  (ctrOfMass ^. term) "CM" [physics]
