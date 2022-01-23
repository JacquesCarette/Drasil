module Drasil.Projectile.Derivations (
  timeDeriv,
  landPosDeriv,
  rectVelDeriv,
  rectPosDeriv,
  horMotionDeriv, horMotionEqn1, horMotionEqn2,
  verMotionDeriv, verMotionEqn1, verMotionEqn2, verMotionEqn3
) where

import Prelude hiding (cos, sin)

import Language.Drasil (eqSymb, LiteralC(..), ModelExprC(..), ExprC(..),
  ModelExpr, square, half)
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed,
  constAccel, ixPos, iyPos)
import Data.Drasil.Quantities.Physics (gravitationalAccel, gravitationalAccelConst,
  iPos, ixVel, iyVel, scalarPos, speed, time, xPos, xVel, yPos, yVel, ixSpeed, iySpeed)

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
                   defint (eqSymb time) (exactDbl 0) (sy time) (sy QP.iSpeed `addRe` (sy QP.constAccel `mulRe` sy time))

--
horMotionDeriv :: [ModelExpr]
horMotionDeriv = [horMotionEqn1, horMotionEqn2]

horMotionEqn1, horMotionEqn2 :: ModelExpr
horMotionEqn1 = sy xVel $= sy ixSpeed
horMotionEqn2 = sy xPos $= sy QP.ixPos `addRe` (sy ixSpeed `mulRe` sy time)

--
verMotionDeriv :: [ModelExpr]
verMotionDeriv = [verMotionEqn1, verMotionEqn2, verMotionEqn3]

verMotionEqn1, verMotionEqn2, verMotionEqn3 :: ModelExpr
verMotionEqn1 = sy yVel $= sy iySpeed $- (sy gravitationalAccel `mulRe` sy time)
verMotionEqn2 = sy yPos $= sy QP.iyPos `addRe` (sy iySpeed `mulRe` sy time) $- (sy gravitationalAccel `mulRe` square (sy time) $/ exactDbl 2)
verMotionEqn3 = square (sy yVel) $= square (sy iySpeed) $- (exactDbl 2 `mulRe` sy gravitationalAccel `mulRe` (sy yPos $- sy QP.iyPos)) 
