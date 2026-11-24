module Drasil.Projectile.Lesson.Review (
  reviewSecs
) where

import qualified Data.Drasil.Concepts.Physics as CP (motion, position, velocity)
import Data.Drasil.Concepts.Math (equation)
import qualified Drasil.Projectile.Expressions as E (lcrectVel, lcrectPos, lcrectNoTime)
import Data.Drasil.Quantities.Physics (speed, time, scalarPos, iPos, iSpeed, constAccel)
import Language.Drasil
import Language.Drasil.Document
import qualified Language.Drasil.Sentence.Combinators as S

reviewSecs :: [Section]
reviewSecs = [reviewHead]

reviewHead :: Section
reviewHead = section (S "Rectilinear Kinematics: Continuous Motion")
  [reviewContextP1, LlC E.lcrectVel, LlC E.lcrectPos, LlC E.lcrectNoTime, reviewEqns, reviewContextP2]
  []
  (makeSecRef "RectKin" $ S "RectKin")

reviewContextP1, reviewEqns, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP_
      [S "As covered previously, the", plural equation, S "relating", phrase CP.velocity, sParen (eS (sy speed)) `sC`
        phrase CP.position, sParen (eS (sy scalarPos)) `S.and_` phrase time, sParen (eS (sy time))
        `S.for` phrase CP.motion `S.in_` S "one dimension with", phrase constAccel,
        sParen (eS (sy constAccel)) +:+ S "are as follows:"]

reviewEqns
  = foldlSP
      [S "where", eS (sy iSpeed) `S.and_` eS (sy iPos),
       S "are the initial", phrase CP.velocity `S.and_` phrase CP.position, S ",respectively"]

reviewContextP2
  = foldlSP
      [S "Only two of these", plural equation, S "are independent" `sC` S "since the third" +:+
       phrase equation, S "can always be derived from the other two"]
