module Drasil.DblPendulum.Concepts where

import Language.Drasil
import Data.Drasil.Domains (physics)
import Data.Drasil.Concepts.Documentation (first, second_)
import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.Theories.Physics (newtonSLRQD)

--below imports needed compoundNC part to work
import Utils.Drasil.Concepts
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force)


concepts :: [IdeaDict]
concepts = nw newtonSLRQD : map nw [rod, horizontal, vertical,
  pendMotion, horizontalPos, verticalPos, horizontalVel,
  verticalVel, horizontalForce, verticalForce, firstRod, secondRod, firstMass, secondMass] 
  ++ map nw defs

rod, horizontal, vertical :: NamedChunk
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 

pendMotion, horizontalPos, verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce, 
  firstRod, secondRod, firstMass, secondMass:: NamedChunk
pendMotion = compoundNC pendulum motion
horizontalPos = compoundNC horizontal position
verticalPos = compoundNC vertical position
horizontalVel = compoundNC horizontal velocity
verticalVel = compoundNC vertical velocity
horizontalForce = compoundNC horizontal force
verticalForce = compoundNC vertical force
firstRod = compoundNC first rod
secondRod = compoundNC second_ rod
firstMass = compoundNC first mass
secondMass = compoundNC second_ mass

defs :: [ConceptChunk]
defs = [arcLen]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"

progName :: CI
progName = commonIdeaWithDict "pendulumTitle" (pn "Pendulum") "Pendulum" [physics]
