module Drasil.DblPend.Concepts (
  pendMotion, rod, ideaDicts, defs, firstRod, secondRod, firstObject,
  secondObject, verticalPos, horizontalPos, horizontalVel, verticalVel,
  horizontalForce, verticalForce, horizontalAccel, verticalAccel, arcLen
) where

import Drasil.Database (mkUid)
import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_, object)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force, acceleration)

ideaDicts :: [IdeaDict]
ideaDicts = [rod, horizontal, vertical,
  pendMotion, horizontalPos, verticalPos, horizontalVel,horizontalAccel, verticalAccel,
  verticalVel, horizontalForce, verticalForce, firstRod, secondRod, firstObject, secondObject]

rod, horizontal, vertical :: IdeaDict
rod = idea' (mkUid "rod") (cn' "rod")
horizontal = idea' (mkUid "horizontal") (cn "horizontal")
vertical = idea' (mkUid "vertical") (cn "vertical")

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
arcLen = cncpt''' (mkUid "arc length") (nounPhraseSP "arc length")
  (S "the distance between two points on a curve")
