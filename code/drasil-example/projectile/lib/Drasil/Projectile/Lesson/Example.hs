module Drasil.Projectile.Lesson.Example where

import Data.Drasil.Concepts.Physics (motion, acceleration, velocity)
import Data.Drasil.Quantities.Physics (ixVel, xVel)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

exampleContent :: [Contents]
exampleContent = [exampleContextP1, exampleC1]

exampleContextP1 :: Contents
exampleContextP1 = foldlSP_ [S "A sack slides off the ramp, shown in Figure",
    S "We can ignore the physics of the sack sliding down the ramp and just focus on its exit", phrase velocity +:+. S "from the ramp",
    S "There is initially no vertical component of", phrase velocity `S.andThe` S "horizontal", phrase velocity, S "is:"]

exampleC1 :: Contents
exampleC1 = unlbldCode (sy xVel $= sy ixVel )