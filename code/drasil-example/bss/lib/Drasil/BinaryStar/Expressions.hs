module Drasil.BinaryStar.Expressions (sepDistExpr,
  accelXExpr_1, accelYExpr_1, accelXExpr_2, accelYExpr_2,
  energyExpr) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Data.Drasil.Quantities.Physics (gravitationalConst, energy)
import Drasil.BinaryStar.Unitals (mass_1, mass_2,
  xPos_1, yPos_1, xPos_2, yPos_2,
  xVel_1, yVel_1, xVel_2, yVel_2, sepDist)

---------------------------------------------------------
-- Separation distance: r₁₂ = sqrt((x₁-x₂)² + (y₁-y₂)²)
---------------------------------------------------------
sepDistExpr :: PExpr
sepDistExpr = sqrt (square (sy xPos_1 $- sy xPos_2)
              $+ square (sy yPos_1 $- sy yPos_2))

---------------------------------------------------------
-- Acceleration of star 1 (from IM)
-- ax₁ = -G * m₂ * (x₁ - x₂) / r₁₂³
-- ay₁ = -G * m₂ * (y₁ - y₂) / r₁₂³
---------------------------------------------------------
accelXExpr_1, accelYExpr_1 :: PExpr
accelXExpr_1 = neg (sy gravitationalConst) $* sy mass_2
  $* (sy xPos_1 $- sy xPos_2)
  $/ (sy sepDist $^ exactDbl 3)

accelYExpr_1 = neg (sy gravitationalConst) $* sy mass_2
  $* (sy yPos_1 $- sy yPos_2)
  $/ (sy sepDist $^ exactDbl 3)

---------------------------------------------------------
-- Acceleration of star 2 (from IM)
-- ax₂ = G * m₁ * (x₁ - x₂) / r₁₂³
-- ay₂ = G * m₁ * (y₁ - y₂) / r₁₂³
---------------------------------------------------------
accelXExpr_2, accelYExpr_2 :: PExpr
accelXExpr_2 = sy gravitationalConst $* sy mass_1
  $* (sy xPos_1 $- sy xPos_2)
  $/ (sy sepDist $^ exactDbl 3)

accelYExpr_2 = sy gravitationalConst $* sy mass_1
  $* (sy yPos_1 $- sy yPos_2)
  $/ (sy sepDist $^ exactDbl 3)

---------------------------------------------------------
-- Total mechanical energy: E = KE₁ + KE₂ - PE
---------------------------------------------------------
energyExpr :: ModelExpr
energyExpr = sy energy $=
  half (sy mass_1 $* (square (sy xVel_1) $+ square (sy yVel_1)))
  $+ half (sy mass_2 $* (square (sy xVel_2) $+ square (sy yVel_2)))
  $- (sy gravitationalConst $* sy mass_1 $* sy mass_2 $/ sy sepDist)
