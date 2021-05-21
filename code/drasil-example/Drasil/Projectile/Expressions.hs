-- Assemble all of the mathematical expressions here, to promote re-use
--
-- This is supposed to always be imported qualified, since we're purposefully
-- overloading the names.
module Drasil.Projectile.Expressions where

import Prelude hiding (cos, sin)

import Language.Drasil
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed,
  constAccel, xConstAccel, yConstAccel, ixPos, iyPos)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, ixVel, iyVel, xPos, yPos, time,
  speed, iPos, scalarPos, xVel, yVel, xAccel, yAccel, position, velocity, acceleration,
  constAccelV)

import Drasil.Projectile.Unitals (launAngle, launSpeed, targPos, tol, landPos, flightDur, offset)

flightDur', iyPos, yConstAccel, iSpeed :: Expr
flightDur' = exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst
iyPos = dbl 0                                   -- launchOrigin
yConstAccel = neg $ sy gravitationalAccelConst  -- accelYGravity
iSpeed = sy launSpeed

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4 :: Expr
timeDerivEqn1 = sy yPos $= (sy iyVel `mulRe` sy time) $- half (sy gravitationalAccelConst `mulRe` square (sy time))
timeDerivEqn2 = (sy iyVel `mulRe` sy flightDur) $- half (sy gravitationalAccelConst `mulRe` square (sy flightDur)) $= exactDbl 0
timeDerivEqn3 = sy iyVel $- half (sy gravitationalAccelConst `mulRe` sy flightDur) $= exactDbl 0
timeDerivEqn4 = sy flightDur $= exactDbl 2 `mulRe` sy iyVel $/ sy gravitationalAccelConst

landPosExpr, landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: Expr
landPosExpr = exactDbl 2 `mulRe` square (sy launSpeed) `mulRe` sin (sy launAngle) `mulRe` cos (sy launAngle) $/ sy gravitationalAccelConst
landPosDerivEqn1 = sy xPos    $= sy ixVel `mulRe` sy time
landPosDerivEqn2 = sy landPos $= sy ixVel `mulRe` exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed `mulRe` cos (sy launAngle) `mulRe` exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst

offset' :: Expr
offset' = sy landPos $- sy targPos

message :: Expr
message = completeCase [case1, case2, case3]
  where case1 = (Str "The target was hit.",        abs_ (sy offset $/ sy targPos) $< sy tol)
        case2 = (Str "The projectile fell short.", sy offset $< dbl 0)
        case3 = (Str "The projectile went long.",  sy offset $> dbl 0)

--
speed' :: Expr
speed' = sy QP.iSpeed `addRe` (sy QP.constAccel `mulRe` sy time)

rectVelDerivEqn1, rectVelDerivEqn2 :: Expr
rectVelDerivEqn1 = sy QP.constAccel $= deriv (sy speed) time
rectVelDerivEqn2 = defint (eqSymb speed) (sy QP.iSpeed) (sy speed) (dbl 1) $=
                   defint (eqSymb time) (dbl 0) (sy time) (sy QP.constAccel)

scalarPos' :: Expr
scalarPos' = sy iPos `addRe` (sy QP.iSpeed `mulRe` sy time `addRe` half (sy QP.constAccel `mulRe` square (sy time)))

rectPosDerivEqn1, rectPosDerivEqn2, rectPosDerivEqn3 :: Expr
rectPosDerivEqn1 = sy speed $= deriv (sy scalarPos) time
rectPosDerivEqn2 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) (dbl 1) $=
                   defint (eqSymb time) (dbl 0) (sy time) (sy speed)
rectPosDerivEqn3 = defint (eqSymb scalarPos) (sy iPos) (sy scalarPos) (dbl 1) $=
                   defint (eqSymb time) (dbl 0) (sy time) (sy QP.iSpeed `addRe` (sy QP.constAccel `mulRe` sy time))

--
velVecExpr :: Expr
velVecExpr = vec2D (sy ixVel `addRe` (sy QP.xConstAccel `mulRe` sy time)) (sy iyVel `addRe` (sy QP.yConstAccel `mulRe` sy time))

--
posVecExpr :: Expr
posVecExpr = vec2D
              (sy QP.ixPos `addRe` (sy ixVel `mulRe` sy time) `addRe` half (sy QP.xConstAccel `mulRe` square (sy time)))
              (sy QP.iyPos `addRe` (sy iyVel `mulRe` sy time) `addRe` half (sy QP.yConstAccel `mulRe` square (sy time)))

-- Helper expressions that represent the vectors of quantities as components
positionXY, velocityXY, accelerationXY, constAccelXY :: Expr
positionXY     = sy position     $= vec2D (sy xPos)           (sy yPos)
velocityXY     = sy velocity     $= vec2D (sy xVel)           (sy yVel)
accelerationXY = sy acceleration $= vec2D (sy xAccel)         (sy yAccel)
constAccelXY   = sy constAccelV  $= vec2D (sy QP.xConstAccel) (sy QP.yConstAccel)
