module Drasil.Projectile.Lesson.Review where

import Data.Drasil.Concepts.Physics (motion, constAccel, oneD)
import qualified Drasil.Projectile.Expressions as E (speed', scalarPos', rectNoTime)
import qualified Data.Drasil.Quantities.Physics as QP (speed, time, scalarPos, iPos, iSpeed, constAccel)
import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

reviewContent :: [Contents]
reviewContent = [reviewContextP1, rectVel, rectPos, reactnoTime, reviewEq, reviewContextP2]

reviewContextP1, reviewEq, rectVel, rectPos, reactnoTime, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP_
      [S "As covered previously, the equations relating velocity (", E (sy QP.speed) +:+ S "), position (", 
        E (sy QP.scalarPos) +:+ S"), and time (", E (sy QP.time) +:+ S ") for",
        phrase motion `S.sIn` S "one dimension with constant acceleration (", E (sy QP.constAccel) +:+
        S ") are as follows:"]

reviewEq 
  = foldlSP 
      [S "where", E (sy QP.iSpeed) `S.sAnd` E (sy QP.iPos), 
       S "are the initial velocity and position, respectively"]

rectVel = eqUnR'(sy QP.speed $= E.speed')
rectPos = eqUnR' (sy QP.scalarPos $= E.scalarPos')
reactnoTime = eqUnR'(E.rectNoTime)

reviewContextP2
  = foldlSP 
      [S "Only two of these equations are independent,",
         S "since the third equation can always be derived from the other two",
       S "[--ref-- is not in the Projectile SRS]"]