module Drasil.Projectile.Derivations (
  timeDeriv,
  landPosDeriv,
  rectVelDeriv,
  rectPosDeriv
) where

import Prelude hiding (cos, sin)

import Language.Drasil (eqSymb, LiteralC(..), ModelExprC(..), ExprC(..),
  ModelExpr, square, half)
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed, constAccel)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, iPos, ixVel, iyVel, scalarPos, speed, time, xPos, yPos)

import Drasil.Projectile.Unitals (launAngle, launSpeed, landPos, flightDur)

--
timeDeriv :: [ModelExpr]
timeDeriv = [timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4]

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4 :: ModelExpr
timeDerivEqn1 = sy yPos $= (sy iyVel $* sy time) $- half (sy gravitationalAccelConst $* square (sy time))
timeDerivEqn2 = (sy iyVel $* sy flightDur) $- half (sy gravitationalAccelConst $* square (sy flightDur)) $= exactDbl 0
timeDerivEqn3 = sy iyVel $- half (sy gravitationalAccelConst $* sy flightDur) $= exactDbl 0
timeDerivEqn4 = sy flightDur $= exactDbl 2 $* sy iyVel $/ sy gravitationalAccelConst

--
landPosDeriv :: [ModelExpr]
landPosDeriv = [landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3]

landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: ModelExpr
landPosDerivEqn1 = sy xPos    $= sy ixVel $* sy time
landPosDerivEqn2 = sy landPos $= sy ixVel $* exactDbl 2 $* sy launSpeed $* sin (sy launAngle) $/ sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed $* cos (sy launAngle) $* exactDbl 2 $* sy launSpeed $* sin (sy launAngle) $/ sy gravitationalAccelConst


--
rectVelDeriv :: [ModelExpr]
rectVelDeriv = [rectVelDerivEqn1, rectVelDerivEqn2]

rectVelDerivEqn1, rectVelDerivEqn2 :: ModelExpr
rectVelDerivEqn1 = sy QP.constAccel $= deriv (sy speed) time
rectVelDerivEqn2 = defint (eqSymb speed) (sy QP.iSpeed) (sy speed) (exactDbl 1) $=
                   defint (eqSymb time) (exactDbl 0) (sy time) (sy QP.constAccel)

--
rectPosDeriv :: [ModelExpr]
rectPosDeriv = [rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3]

rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3 :: ModelExpr
rectPosDerivEqn1 = sy speed $= deriv (sy scalarPos) time
rectPosDerivEqn2 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) (exactDbl 1) $=
                   defint (eqSymb time) (exactDbl 0) (sy time) (sy speed)
rectPosDerivEqn3 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) (exactDbl 1) $=
                   defint (eqSymb time) (exactDbl 0) (sy time) (sy QP.iSpeed $+ (sy QP.constAccel $* sy time))
