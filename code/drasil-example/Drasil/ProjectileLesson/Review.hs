module Drasil.ProjectileLesson.Review where

import qualified Drasil.ProjectileLesson.Expressions as E (rectVel, rectPos, rectNoTime)
import qualified Data.Drasil.Quantities.Physics QP (iPos, iSpeed)
import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

reviewContextP1, reviewEq, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP
      [S "As covered previously, the equations relating velocity( 𝑣 ), position ( 𝑝 )",
       S "and time ( 𝑡 ) for motion in one dimension with constant acceleration ( 𝑎𝑐 ) are as follows:",]

reviewEq 
  = foldlSP 
      [E.rectVel, E.rectPos, E.rectNoTime,
       S "where", E (sy QP.iSpeed) `S.sAnd` E (sy QP.iPos), 
       S "are the initial velocity and position, respectively"]

reviewContextP2
  = foldlSP 
      [S "Only two of these equations are independent,",
         S "since the third equation can always be derived from the other two",
       S "[" makeRef2S E.rectVel, S "is not in the Projectile SRS]"]