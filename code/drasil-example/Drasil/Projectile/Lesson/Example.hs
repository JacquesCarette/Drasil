module Drasil.Projectile.Lesson.Example where

import Data.Drasil.Concepts.Physics (motion, acceleration, velocity)
import Data.Drasil.Quantities.Physics (gravitationalAccel) 
import Data.Drasil.Concepts.Math (component)

import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

expIntro :: Contents
expIntro = foldlSP_ 
  [S "A sack slides off the ramp, shown in Figure (ref).", S "We can ignore the physics" `S.of_` 
   S "the sack sliding down the ramp and just focus on its exit", phrase velocity +:+. S "from the ramp", 
   S "There is initially no vertical", phrase component `S.of_` phrase velocity, S "and the  horizontal",
   phrase velocity +:+ S "is:"]

grav :: Expr
grav = sy gravitationalAccel $= dbl 9.81 