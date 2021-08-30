-- Assemble all of the mathematical expressions here, to promote re-use
--
-- This is supposed to always be imported qualified, since we're purposefully
-- overloading the names.
module Drasil.Projectile.Expressions where

import Prelude hiding (cos, sin)

import Language.Drasil
import Utils.Drasil
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed,
  constAccel, xConstAccel, yConstAccel, ixPos, iyPos)
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, 
  ixVel, iyVel, xPos, yPos, time, iPos, scalarPos, xVel, yVel, xAccel, yAccel, position, velocity, acceleration,
  constAccelV, speed)

import Drasil.Projectile.Unitals (launAngle, launSpeed, targPos, tol, landPos, offset)

flightDur', iyPos, yConstAccel, iSpeed :: Expr
flightDur' = exactDbl 2 `mulRe` sy launSpeed `mulRe` sin (sy launAngle) $/ sy gravitationalAccelConst
iyPos = exactDbl 0                              -- launchOrigin
yConstAccel = neg $ sy gravitationalAccelConst  -- accelYGravity
iSpeed = sy launSpeed

offset' :: Expr
offset' = sy landPos $- sy targPos

message :: Expr
message = completeCase [case1, case2, case3]
  where case1 = (str "The target was hit.",        abs_ (sy offset $/ sy targPos) $< sy tol)
        case2 = (str "The projectile fell short.", sy offset $< exactDbl 0)
        case3 = (str "The projectile went long.",  sy offset $> exactDbl 0)

--
speed' :: Expr
speed' = sy QP.iSpeed `addRe` (sy QP.constAccel `mulRe` sy time)

scalarPos' :: Expr
scalarPos' = sy iPos `addRe` (sy QP.iSpeed `mulRe` sy time `addRe` half (sy QP.constAccel `mulRe` square (sy time)))

rectNoTime :: Expr
rectNoTime = square (sy speed) $= square (sy QP.iSpeed) `addRe` (exactDbl 2 `mulRe` sy QP.constAccel `mulRe` (sy scalarPos $- sy iPos))

--
velVecExpr :: Expr
velVecExpr = vec2D (sy ixVel `addRe` (sy QP.xConstAccel `mulRe` sy time)) (sy iyVel `addRe` (sy QP.yConstAccel `mulRe` sy time))

--
posVecExpr :: Expr
posVecExpr = vec2D
              (sy QP.ixPos `addRe` (sy ixVel `mulRe` sy time) `addRe` half (sy QP.xConstAccel `mulRe` square (sy time)))
              (sy QP.iyPos `addRe` (sy iyVel `mulRe` sy time) `addRe` half (sy QP.yConstAccel `mulRe` square (sy time)))

--
landPosExpr :: Expr
landPosExpr = exactDbl 2 `mulRe` square (sy launSpeed) `mulRe` sin (sy launAngle) `mulRe` cos (sy launAngle) $/ sy gravitationalAccelConst

-- Helper expressions that represent the vectors of quantities as components
positionXY, velocityXY, accelerationXY, constAccelXY :: Expr
positionXY     = sy position     $= vec2D (sy xPos)           (sy yPos)
velocityXY     = sy velocity     $= vec2D (sy xVel)           (sy yVel)
accelerationXY = sy acceleration $= vec2D (sy xAccel)         (sy yAccel)
constAccelXY   = sy constAccelV  $= vec2D (sy QP.xConstAccel) (sy QP.yConstAccel)

-- Expressions for lesson
lcrectVel, lcrectPos, lcrectNoTime :: LabelledContent
lcrectVel = lbldExpr (sy speed $= speed') (makeEqnRef "rectVel")
lcrectPos = lbldExpr (sy scalarPos $= scalarPos') (makeEqnRef "rectPos")
lcrectNoTime = lbldExpr rectNoTime (makeEqnRef "rectNoTime")

-- References --
eqnRefs :: [Reference]
eqnRefs = map ref [lcrectVel, lcrectPos, lcrectNoTime]
