module Drasil.Projectile.Lesson.Review where

import Data.Drasil.Concepts.Physics (motion, position, time, velocity)
import Data.Drasil.Concepts.Math (equation)
import qualified Drasil.Projectile.Expressions as E (lcrectVel, lcrectPos, lcrectNoTime)
import qualified Data.Drasil.Quantities.Physics as QP (speed, time, scalarPos, iPos, iSpeed, constAccel)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

reviewContent :: [Contents]
reviewContent = [reviewHead, reviewContextP1, LlC E.lcrectVel, LlC E.lcrectPos, LlC E.lcrectNoTime, reviewEqns, reviewContextP2]

reviewHead, reviewContextP1, reviewEqns, reviewContextP2 :: Contents
reviewHead = foldlSP_ [headSent 2 (S "Rectilinear Kinematics: Continuous Motion")]
reviewContextP1
  = foldlSP_
      [S "As covered previously, the", plural equation, S "relating", phrase velocity, sParen (eS (sy QP.speed)) `sC` 
        phrase position, sParen (eS (sy QP.scalarPos)) `S.and_` phrase time, sParen (eS (sy QP.time)) 
        `S.for` phrase motion `S.in_` S "one dimension with", phrase QP.constAccel, 
        sParen (eS (sy QP.constAccel)) +:+ S "are as follows:"]

reviewEqns 
  = foldlSP 
      [S "where", eS (sy QP.iSpeed) `S.and_` eS (sy QP.iPos), 
       S "are the initial", phrase velocity `S.and_` phrase position, S ",respectively"]

reviewContextP2
  = foldlSP 
      [S "Only two of these", plural equation, S "are independent, since the third" +:+
       phrase equation, S "can always be derived from the other two"]


