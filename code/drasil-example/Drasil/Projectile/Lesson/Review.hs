module Drasil.Projectile.Lesson.Review where

import qualified Drasil.Projectile.Expressions as E (speed', scalarPos', rectNoTime)
import qualified Data.Drasil.Quantities.Physics as QP (speed, scalarPos, iPos, iSpeed)
import Language.Drasil
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

reviewContextP1, reviewEq, reviewContextP2 :: Contents
reviewContextP1
  = foldlSP
      [S "As covered previously, the equations relating velocity( 𝑣 ), position ( 𝑝 )",
       S "and time ( 𝑡 ) for motion in one dimension with constant acceleration ( 𝑎𝑐 ) are as follows:"]

reviewEq 
  = foldlSP 
      [E (sy QP.speed $= E.speed'),
       S "where", E (sy QP.iSpeed) `S.sAnd` E (sy QP.iPos), 
       S "are the initial velocity and position, respectively"]

reviewEqns :: [Expr]
reviewEqns = [sy QP.speed $= E.speed', sy QP.scalarPos $= E.scalarPos', E.rectNoTime]

reviewContextP2
  = foldlSP 
      [S "Only two of these equations are independent,",
         S "since the third equation can always be derived from the other two",
       S "[--ref-- is not in the Projectile SRS]"]