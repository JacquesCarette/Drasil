module Drasil.DblPend.Concepts where

import Language.Drasil
import Data.Drasil.Domains (physics)
import Data.Drasil.Concepts.Documentation (first, second_, object)
import Data.Drasil.Theories.Physics (newtonSLRQD)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force, acceleration)


concepts :: [IdeaDict]
concepts = nw newtonSLRQD : map nw [rod, horizontal, vertical,
  pendMotion, horizontalPos, verticalPos, horizontalVel,horizontalAccel, verticalAccel,
  verticalVel, horizontalForce, verticalForce, firstRod, secondRod, firstObject, secondObject] 
  ++ map nw defs

rod, horizontal, vertical :: IdeaDict
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal") 
vertical = nc "vertical" (cn "vertical") 

pendMotion, horizontalPos, verticalPos, horizontalVel, verticalVel, horizontalForce, verticalForce,
  horizontalAccel, verticalAccel, firstRod, secondRod, firstObject, secondObject:: IdeaDict
pendMotion      = compoundNC pendulum motion
horizontalPos   = compoundNC horizontal position
verticalPos     = compoundNC vertical position
horizontalVel   = compoundNC horizontal velocity
verticalVel     = compoundNC vertical velocity
horizontalAccel = compoundNC horizontal acceleration
verticalAccel   = compoundNC vertical acceleration
horizontalForce = compoundNC horizontal force
verticalForce   = compoundNC vertical force
firstRod        = compoundNC first rod
secondRod       = compoundNC second_ rod
firstObject     = compoundNC first object
secondObject    = compoundNC second_ object

defs :: [ConceptChunk]
defs = [arcLen]

arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length") "the distance between two points on a curve"

progName :: CI
progName = commonIdeaWithDict "dblPendulum" (pn "Double Pendulum") "DblPend" [physics]
