module Drasil.BinaryStar.Expressions where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil

import Drasil.BinaryStar.Unitals (mass_1, mass_2,
  xPos_1, yPos_1, xPos_2, yPos_2, sepDist)

-- | Gravitational constant G  (will be replaced by a proper constant later)
-- G = 6.674e-11 m³/(kg·s²)
gravitationalConst :: PExpr
gravitationalConst = dbl 6.674e-11

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
accelXExpr_1 = neg gravitationalConst $* sy mass_2
  $* (sy xPos_1 $- sy xPos_2)
  $/ (sy sepDist $^ exactDbl 3)

accelYExpr_1 = neg gravitationalConst $* sy mass_2
  $* (sy yPos_1 $- sy yPos_2)
  $/ (sy sepDist $^ exactDbl 3)

---------------------------------------------------------
-- Acceleration of star 2 (from IM)
-- ax₂ = G * m₁ * (x₁ - x₂) / r₁₂³
-- ay₂ = G * m₁ * (y₁ - y₂) / r₁₂³
---------------------------------------------------------
accelXExpr_2, accelYExpr_2 :: PExpr
accelXExpr_2 = gravitationalConst $* sy mass_1
  $* (sy xPos_1 $- sy xPos_2)
  $/ (sy sepDist $^ exactDbl 3)

accelYExpr_2 = gravitationalConst $* sy mass_1
  $* (sy yPos_1 $- sy yPos_2)
  $/ (sy sepDist $^ exactDbl 3)
