
{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.Concepts where

import Language.Drasil
import Data.Drasil.Concepts.Documentation (first, second_, object)
import Language.Drasil.Chunk.Concept.NamedCombinators (compoundNC)
import Data.Drasil.Concepts.Physics (pendulum, motion, position, velocity, force, acceleration)

-- | Basic Concepts
rod, horizontal, vertical :: IdeaDict
rod = nc "rod" (cn' "rod")
horizontal = nc "horizontal" (cn "horizontal")
vertical = nc "vertical" (cn "vertical")

-- | Composite Concepts
firstRod, secondRod, firstObject, secondObject :: IdeaDict
firstRod    = compoundNC first rod
secondRod   = compoundNC second_ rod
firstObject = compoundNC first object
secondObject = compoundNC second_ object

-- | Motion and Position Concepts
pendMotion, horizontalPos, verticalPos :: IdeaDict
pendMotion    = compoundNC pendulum motion
horizontalPos = compoundNC horizontal position
verticalPos   = compoundNC vertical position

horizontalVel, verticalVel :: IdeaDict
horizontalVel = compoundNC horizontal velocity
verticalVel   = compoundNC vertical velocity

horizontalAccel, verticalAccel :: IdeaDict
horizontalAccel = compoundNC horizontal acceleration
verticalAccel   = compoundNC vertical acceleration

horizontalForce, verticalForce :: IdeaDict
horizontalForce = compoundNC horizontal force
verticalForce   = compoundNC vertical force

-- | Vector Quantities (not component-wise)
pendulumPos, pendulumVel, pendulumAccel, pendulumForce :: IdeaDict
pendulumPos   = compoundNC pendulum position
pendulumVel   = compoundNC pendulum velocity
pendulumAccel = compoundNC pendulum acceleration
pendulumForce = compoundNC pendulum force

-- | Geometric Algebra Concepts
vectorQuantity, multivectorQuantity :: IdeaDict
vectorQuantity      = nc "vectorQuantity" (cn "vector quantity")
multivectorQuantity = nc "multivectorQuantity" (cn "multivector quantity")

cliffordAlgebra, geometricProduct :: IdeaDict
cliffordAlgebra  = nc "cliffordAlgebra" (cn "Clifford algebra")
geometricProduct = nc "geometricProduct" (cn "geometric product")

-- | Collections
concepts :: [IdeaDict]
concepts =
  [ rod, firstRod, secondRod, firstObject, secondObject
  , pendMotion
  , pendulumPos, pendulumVel, pendulumAccel, pendulumForce
  , vectorQuantity, multivectorQuantity
  , cliffordAlgebra, geometricProduct
  , horizontalPos, verticalPos
  , horizontal, vertical
  , horizontalVel, verticalVel
  , horizontalAccel, verticalAccel
  , horizontalForce, verticalForce
  ]

-- | Example ConceptChunk (can appear in docs)
arcLen :: ConceptChunk
arcLen = dcc "arc length" (nounPhraseSP "arc length")
  "the distance between two points on a curve"


