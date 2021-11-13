module Drasil.Projectile.Lesson.Review where

import Data.Drasil.Concepts.Physics (motion, position, time)
import qualified Drasil.Projectile.Expressions as E (lcrectVel, lcrectPos, lcrectNoTime)
import Drasil.Projectile.Concepts (projectile)
import qualified Data.Drasil.Quantities.Physics as QP (speed, time, scalarPos, iPos, iSpeed, constAccel)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S

reviewContent :: [Contents]
reviewContent = [reviewContextP1, LlC E.lcrectVel, LlC E.lcrectPos, LlC E.lcrectNoTime, reviewEqns, reviewContextP2]

reviewContextP1, reviewEqns, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP_
      [S "As covered previously, the equations relating velocity", sParen (eS (sy QP.speed)) `sC` 
        phrase position, sParen (eS (sy QP.scalarPos)) `S.and_` phrase time, sParen (eS (sy QP.time)) 
        `S.for` phrase motion `S.in_` S "one dimension with constant acceleration", 
        sParen (eS (sy QP.constAccel)) +:+ S "are as follows:"]

reviewEqns 
  = foldlSP 
      [S "where", eS (sy QP.iSpeed) `S.and_` eS (sy QP.iPos), 
       S "are the initial velocity and position, respectively"]

reviewContextP2
  = foldlSP 
      [S "Only two of these equations are independent,",
         S "since the third equation can always be derived from the other two.",
       S "[", refS E.lcrectNoTime +:+ S "is not in the", atStart projectile, S"SRS]"]


