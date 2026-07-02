module Drasil.BinaryStar.ODEs (bssODEOpts, bssODEInfo) where

import Language.Drasil (ExprC(..), LiteralC(int, exactDbl, dbl), square, sqrt)
import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)

import Data.Drasil.Quantities.Physics (time)

import Drasil.BinaryStar.Unitals (mass_1, mass_2, bssStateVar,
  xPos_1_0, yPos_1_0, xPos_2_0, yPos_2_0,
  xVel_1_0, yVel_1_0, xVel_2_0, yVel_2_0, tFinal)
import Prelude hiding (sqrt)

bssODEOpts :: ODEOptions
bssODEOpts = odeOptions RK45 (dbl 1e-8) (dbl 1e-8) (dbl 10.0)

bssODEInfo :: ODEInfo
bssODEInfo = odeInfo
  (quantvar time)              -- independent variable: t
  (quantvar bssStateVar)       -- dependent variable: state vector
  [quantvar mass_1, quantvar mass_2,
   quantvar xPos_1_0, quantvar yPos_1_0, quantvar xPos_2_0, quantvar yPos_2_0,
   quantvar xVel_1_0, quantvar yVel_1_0, quantvar xVel_2_0, quantvar yVel_2_0,
   quantvar tFinal]  -- other variables (read from input)
  (exactDbl 0)                 -- t_init = 0
  (sy tFinal)                  -- t_final from input
  -- Initial values: [x1_0, y1_0, x2_0, y2_0, vx1_0, vy1_0, vx2_0, vy2_0]
  [sy xPos_1_0, sy yPos_1_0, sy xPos_2_0, sy yPos_2_0,
   sy xVel_1_0, sy yVel_1_0, sy xVel_2_0, sy yVel_2_0]
  -- ODE system: 8 first-order equations
  [ vx1,                                                       -- dx1/dt = vx1
    vy1,                                                       -- dy1/dt = vy1
    vx2,                                                       -- dx2/dt = vx2
    vy2,                                                       -- dy2/dt = vy2
    neg gConst $* m2 $* (x1 $- x2) $/ (r12 $^ exactDbl 3),   -- dvx1/dt
    neg gConst $* m2 $* (y1 $- y2) $/ (r12 $^ exactDbl 3),   -- dvy1/dt
    gConst $* m1 $* (x1 $- x2) $/ (r12 $^ exactDbl 3),       -- dvx2/dt
    gConst $* m1 $* (y1 $- y2) $/ (r12 $^ exactDbl 3)        -- dvy2/dt
  ]
  bssODEOpts
  where
    -- State vector: [x1=0, y1=1, x2=2, y2=3, vx1=4, vy1=5, vx2=6, vy2=7]
    x1  = idx (sy bssStateVar) (int 0)
    y1  = idx (sy bssStateVar) (int 1)
    x2  = idx (sy bssStateVar) (int 2)
    y2  = idx (sy bssStateVar) (int 3)
    vx1 = idx (sy bssStateVar) (int 4)
    vy1 = idx (sy bssStateVar) (int 5)
    vx2 = idx (sy bssStateVar) (int 6)
    vy2 = idx (sy bssStateVar) (int 7)
    m1  = sy mass_1
    m2  = sy mass_2
    -- Hardcoded G due to Drasil bug #2998
    gConst = dbl 6.6743e-11
    r12 = sqrt (square (x1 $- x2) $+ square (y1 $- y2))
