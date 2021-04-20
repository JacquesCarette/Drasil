-- Assemble all of the mathematical expressions here, to promote re-use
--
-- This is supposed to always be imported qualified, since we're purposefully
-- overloading the names.
module Drasil.Projectile.Expressions where

import Prelude hiding (cos, sin)

import Language.Drasil
import Data.Drasil.Quantities.Physics (gravitationalAccelConst, ixVel, iyVel, xPos, yPos, time)

import Drasil.Projectile.Unitals (launAngle, launSpeed, targPos, tol, landPos, flightDur, offset)

flightDur', iyPos, yConstAccel, iSpeed :: Expr
flightDur' = 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
iyPos = 0                                   --  launchOrigin
yConstAccel = - sy gravitationalAccelConst  -- accelYGravity
iSpeed = sy launSpeed

timeDerivEqn1, timeDerivEqn2, timeDerivEqn3, timeDerivEqn4 :: Expr
timeDerivEqn1 = sy yPos $= sy iyVel * sy time - sy gravitationalAccelConst * square (sy time) / 2
timeDerivEqn2 = sy iyVel * sy flightDur - sy gravitationalAccelConst * square (sy flightDur) / 2 $= 0
timeDerivEqn3 = sy iyVel - sy gravitationalAccelConst * sy flightDur / 2 $= 0
timeDerivEqn4 = sy flightDur $= 2 * sy iyVel / sy gravitationalAccelConst

landPosExpr, landPosDerivEqn1, landPosDerivEqn2, landPosDerivEqn3 :: Expr
landPosExpr = 2 * square (sy launSpeed) * sin (sy launAngle) * cos (sy launAngle) / sy gravitationalAccelConst
landPosDerivEqn1 = sy xPos    $= sy ixVel * sy time
landPosDerivEqn2 = sy landPos $= sy ixVel * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst
landPosDerivEqn3 = sy landPos $= sy launSpeed * cos (sy launAngle) * 2 * sy launSpeed * sin (sy launAngle) / sy gravitationalAccelConst

offset' :: Expr
offset' = sy landPos - sy targPos

message :: Expr
message = completeCase [case1, case2, case3]
  where case1 = (Str "The target was hit.",        abs (sy offset / sy targPos) $< sy tol)
        case2 = (Str "The projectile fell short.", sy offset $< 0)
        case3 = (Str "The projectile went long.",  sy offset $> 0)
