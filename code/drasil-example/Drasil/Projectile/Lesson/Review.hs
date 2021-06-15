module Drasil.Projectile.Lesson.Review where

import Data.Drasil.Concepts.Physics (motion, position, time)
import qualified Drasil.Projectile.Expressions as E (speed', scalarPos', rectNoTime)
import Drasil.Projectile.Concepts (projectile)
import qualified Data.Drasil.Quantities.Physics as QP (speed, time, scalarPos, iPos, iSpeed, constAccel)
import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

reviewContent :: [Contents]
reviewContent = [reviewContextP1, LlC rectVel, LlC rectPos, LlC reactnoTime, reviewEqns, reviewContextP2]

reviewContextP1, reviewEqns, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP_
      [S "As covered previously, the equations relating velocity", sParen (E (sy QP.speed)) `sC` 
        phrase position, sParen (E (sy QP.scalarPos)) `S.sAnd` phrase time, sParen (E (sy QP.time)) 
        `S.sFor` phrase motion `S.sIn` S "one dimension with constant acceleration", 
        sParen (E (sy QP.constAccel)) +:+ S "are as follows:"]

reviewEqns 
  = foldlSP 
      [S "where", E (sy QP.iSpeed) `S.sAnd` E (sy QP.iPos), 
       S "are the initial velocity and position, respectively"]

reviewContextP2
  = foldlSP 
      [S "Only two of these equations are independent,",
         S "since the third equation can always be derived from the other two.",
       S "[", makeRef2S reactnoTime +:+ S "is not in the", atStart projectile, S"SRS]"]

rectVel, rectPos, reactnoTime :: LabelledContent
rectVel = eqUnR (sy QP.speed $= E.speed') (makeEqnRef "rectVel")
rectPos = eqUnR (sy QP.scalarPos $= E.scalarPos') (makeEqnRef "rectPos")
reactnoTime = eqUnR (E.rectNoTime) (makeEqnRef "reactNoTime")

