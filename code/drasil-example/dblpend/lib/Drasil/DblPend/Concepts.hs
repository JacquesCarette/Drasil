{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_, object)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force, acceleration)

-- | Basic IdeaDicts
rod, horizontal, vertical :: IdeaDict
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal")
vertical = nc "vertical" (cn "vertical")

pendMotion, horizontalPos, verticalPos, horizontalVel, verticalVel,
  horizontalForce, verticalForce, horizontalAccel, verticalAccel,
  firstRod, secondRod, firstObject, secondObject :: IdeaDict
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

-- | Collection of all IdeaDicts
concepts :: [IdeaDict]
concepts = [ rod, horizontal, vertical
           , pendMotion, horizontalPos, verticalPos
           , horizontalVel, verticalVel
           , horizontalAccel, verticalAccel
           , horizontalForce, verticalForce
           , firstRod, secondRod, firstObject, secondObject
           ]

-- | ConceptChunk for arc length
arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length")
  "the distance between two points on a curve"

-- | No GA concepts included
gaConceptChunks :: [ConceptChunk]
gaConceptChunks = []