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
timeDerivEqn1 = sy yPos $= (sy iyVel `mulRe` sy time) $- half (sy gravitationalAccelConst `mulRe` square (sy time))
timeDerivEqn2 = (sy iyVel `mulRe` sy flightDur) $- half (sy gravitationalAccelConst `mulRe` square (sy flightDur)) $= exactDbl 0
timeDerivEqn3 = sy iyVel $- half (sy gravitationalAccelConst `mulRe` sy flightDur) $= exactDbl 0
timeDerivEqn4 = sy flightDur $= exactDbl 2 `mulRe` sy iyVel $/ sy gravitationalAccelConst

--
landPosDeriv :: [ModelExpr]
landPosDeriv = [landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3]

landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: ModelExpr
landPosDerivEqn1 = sy xPos    $= sy ixVel `mulRe` sy time
landPosDerivEqn2 = sy landPos $= sy ixVel `mulRe` exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed `mulRe` cos (sy launAngle) `mulRe` exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst


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
                   defint (eqSymb time) (exactDbl 0) (sy time) (sy QP.iSpeed `add` (sy QP.constAccel `mulRe` sy time))
