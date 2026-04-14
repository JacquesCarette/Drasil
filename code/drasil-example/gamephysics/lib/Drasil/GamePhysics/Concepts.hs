module Drasil.GamePhysics.Concepts (centreMass, threeD, twoD) where

import Control.Lens ((^.))

import Language.Drasil (commonIdeaWithDict, CI, NamedIdea(..))
import Data.Drasil.Domains (physics)
import Data.Drasil.Concepts.PhysicalProperties (ctrOfMass)
import Data.Drasil.Concepts.Physics (threeD, twoD)

centreMass :: CI
centreMass  = commonIdeaWithDict "centreMass"  (ctrOfMass ^. term) "CM" [physics]
