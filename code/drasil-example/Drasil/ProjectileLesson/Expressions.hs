module Drasil.ProjectileLesson.Expressions where

import Language.Drasil
import Data.Drasil.Quantities.Physics (position, iPos, velocity, iSpeed, time, acceleration, constAccel)

rectVel, rectPos, rectNoTime :: Expr
rectVel = sy velocity $= sy iSpeed + sy constAccel * sy time
rectPos = sy position $= sy iPos + sy iSpeed * sy time + sy constAccel * square (sy time) / 2
rectNoTime = square (sy velocity) $= square (sy iSpeed) + 2 * sy constAccel * (sy position - sy iPos)