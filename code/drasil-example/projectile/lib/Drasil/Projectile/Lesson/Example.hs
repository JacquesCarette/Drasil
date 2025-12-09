module Drasil.Projectile.Lesson.Example where

import Data.Drasil.Concepts.Physics (velocity, height, time, acceleration, gravity, horizontalMotion)
import qualified Data.Drasil.Quantities.Physics as QP (height, gravitationalAccel)
import Data.Drasil.Units.Physics (velU)
import Language.Drasil.ShortHands (cR, lG)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

exampleContent :: [Contents]
exampleContent = [exampleContextP1, codeC1, exampleContextP2, codeC2, exampleContextP3, codeC3]

exampleContextP1, exampleContextP2, exampleContextP3 :: Contents
exampleContextP1 = foldlSP_ [S "A sack slides off the ramp" `sC` S "shown in Figure.",
    S "We can ignore the physics" `S.ofThe` S "sack sliding down the ramp and just focus on its exit", phrase velocity +:+. S "from the ramp",
    S "There is initially no vertical component" `S.of_` phrase velocity `S.andThe` S "horizontal", phrase velocity, S "is:"]
exampleContextP2 = foldlSP_ [S "The", phrase height `S.ofThe` S "ramp from the floor is"]
exampleContextP3 = foldlSP_ [S "Task: Determine the", phrase time, S "needed for the sack to strike the floor and the range",
    P cR +:+. S "where sacks begin to pile up",
    S "The", phrase acceleration, S "due to", phrase gravity, P lG +:+. S "is assumed to have the following value"]

codeC1, codeC2, codeC3 :: Contents
codeC1 = unlbldCode (sy horiz_velo $= exactDbl 17)
codeC2 = unlbldCode (sy QP.height $= exactDbl 6)
codeC3 = unlbldCode (sy QP.gravitationalAccel $= dbl 9.81)

horiz_velo :: UnitalChunk
horiz_velo = uc horizontalMotion (variable "horiz_velo") Real velU
