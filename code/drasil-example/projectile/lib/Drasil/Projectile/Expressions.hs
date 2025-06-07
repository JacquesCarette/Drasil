-- Assemble all of the mathematical expressions here, to promote re-use
--
-- This is supposed to always be imported qualified, since we're purposefully
-- overloading the names.
module Drasil.Projectile.Expressions where

import Prelude hiding (cos, sin)

import Control.Lens ((^.))

import Language.Drasil
import qualified Data.Drasil.Quantities.Physics as QP (iSpeed,
  constAccel, xConstAccel, yConstAccel, ixPos, iyPos)
import Data.Drasil.Quantities.Physics (gravitationalMagnitude,
  ixVel, iyVel, xPos, yPos, time, iPos, scalarPos, xVel, yVel, xAccel, yAccel, position, 
  velocity, acceleration, constAccelV, speed)
import Drasil.Projectile.Unitals (launAngle, launSpeed, targPos, tol, landPos, offset)

flightDur', iyPos, yConstAccel, iSpeed :: PExpr
flightDur' = exactDbl 2 $* sy launSpeed $* sin (sy launAngle) $/ sy gravitationalMagnitude
iyPos = exactDbl 0                              -- launchOrigin
yConstAccel = neg $ sy gravitationalMagnitude  -- accelYGravity
iSpeed = sy launSpeed

offset' :: PExpr
offset' = sy landPos $- sy targPos

message :: PExpr
message = completeCase [case1, case2, case3]
  where case1 = (str "The target was hit.",        abs_ (sy offset $/ sy targPos) $< sy (tol ^. defLhs))
        case2 = (str "The projectile fell short.", sy offset $< exactDbl 0)
        case3 = (str "The projectile went long.",  sy offset $> exactDbl 0)

--
speed' :: PExpr
speed' = sy QP.iSpeed $+ (sy QP.constAccel $* sy time)

scalarPos' :: PExpr
scalarPos' = sy iPos $+ (sy QP.iSpeed $* sy time $+ half (sy QP.constAccel $* square (sy time)))

rectNoTime :: PExpr
rectNoTime = square (sy speed) $= square (sy QP.iSpeed) $+ (exactDbl 2 $* sy QP.constAccel $* (sy scalarPos $- sy iPos))

--
velVecExpr :: PExpr
velVecExpr = vec2D (sy ixVel $+ (sy QP.xConstAccel $* sy time)) (sy iyVel $+ (sy QP.yConstAccel $* sy time))

--
posVecExpr :: PExpr
posVecExpr = vec2D
              (sy QP.ixPos $+ (sy ixVel $* sy time) $+ half (sy QP.xConstAccel $* square (sy time)))
              (sy QP.iyPos $+ (sy iyVel $* sy time) $+ half (sy QP.yConstAccel $* square (sy time)))

--
landPosExpr :: PExpr
landPosExpr = exactDbl 2 $* square (sy launSpeed) $* sin (sy launAngle) $* cos (sy launAngle) $/ sy gravitationalMagnitude

-- Helper expressions that represent the vectors of quantities as components
positionXY, velocityXY, accelerationXY, constAccelXY :: PExpr
positionXY     = sy position     $= vec2D (sy xPos)           (sy yPos)
velocityXY     = sy velocity     $= vec2D (sy xVel)           (sy yVel)
accelerationXY = sy acceleration $= vec2D (sy xAccel)         (sy yAccel)
constAccelXY   = sy constAccelV  $= vec2D (sy QP.xConstAccel) (sy QP.yConstAccel)

-- Expressions for Lesson
horizVel, horizPos :: PExpr
horizVel = sy xVel $= sy ixVel
horizPos = sy xPos $= sy QP.ixPos $+ (sy ixVel $* sy time)

vertVel, vertPos, vertNoTime :: PExpr
vertVel = sy yVel $= sy iyVel $- (sy gravitationalMagnitude $* sy time)
vertPos = sy yPos $= sy QP.iyPos $+ (sy iyVel $* sy time) $- (sy gravitationalMagnitude $* square (sy time) $/ exactDbl 2)
vertNoTime = square (sy yVel) $= square (sy iyVel) $- (exactDbl 2 $* sy gravitationalMagnitude $* (sy yPos $- sy QP.iyPos)) 

lcrectVel, lcrectPos, lcrectNoTime, lchorizVel, lchorizPos, lcvertVel, lcvertPos, lcvertNoTime :: LabelledContent
lcrectVel = lbldExpr (sy speed $= speed') (makeEqnRef "rectVel")
lcrectPos = lbldExpr (sy scalarPos $= scalarPos') (makeEqnRef "rectPos")
lcrectNoTime = lbldExpr rectNoTime (makeEqnRef "rectNoTime")
lchorizVel = lbldExpr horizVel (makeEqnRef "horizVel")
lchorizPos = lbldExpr horizPos (makeEqnRef "horizPos")
lcvertVel = lbldExpr vertVel (makeEqnRef "vertVel")
lcvertPos = lbldExpr vertPos (makeEqnRef "vertPos")
lcvertNoTime = lbldExpr vertNoTime (makeEqnRef "vertNoTime")

-- References --
eqnRefs :: [Reference]
eqnRefs = map ref [lcrectVel, lcrectPos, lcrectNoTime, lchorizVel, lchorizPos, lcvertVel, lcvertPos, lcvertNoTime]
