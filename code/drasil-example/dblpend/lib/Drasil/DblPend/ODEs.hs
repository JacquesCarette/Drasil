module Drasil.DblPend.ODEs (dblPenODEOpts, dblPenODEInfo) where

import Language.Drasil (ExprC(..), LiteralC(int, exactDbl, dbl), square, Expr)
import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)
import Drasil.Code.CodeExpr (CodeExpr, expr)

import Data.Drasil.Quantities.Physics (time)

import Drasil.DblPend.Unitals(massObj_1, massObj_2, lenRod_1, lenRod_2, pendDisAngle,
  angularVel_1, angularAccel_1, angularVel_2, angularAccel_2)
import Prelude hiding (sin, cos)

dblPenODEOpts :: ODEOptions
dblPenODEOpts = odeOptions RK45 (dbl 0.000001) (dbl 0.000001) (dbl 0.001) -- java ode require smaller than 0.001

dblPenODEInfo :: ODEInfo
dblPenODEInfo = odeInfo
  (quantvar time)
  (quantvar pendDisAngle)
  [quantvar massObj_1, quantvar massObj_2, quantvar lenRod_1, quantvar lenRod_2]
  (exactDbl 0)
  (exactDbl 20) -- final time
    [dbl 1.3463968515384828, exactDbl 0, dbl 2.356194490192345, exactDbl 0] -- unit in radian [3*pi/7, 0, 3*pi/4, 0]
  [
    sy angularVel_1,      -- d(theta1)/dt = angular velocity 1
    sy angularAccel_1,    -- d(omega1)/dt = angular acceleration 1
    sy angularVel_2,      -- d(theta2)/dt = angular velocity 2
    sy angularAccel_2     -- d(omega2)/dt = angular acceleration 2
  ]
  dblPenODEOpts
    where t1  = idx (sy pendDisAngle) (int 0) :: Expr -- t1 is theta 1
          o1  = idx (sy pendDisAngle) (int 1) :: Expr -- o1 is omega 1
          t2  = idx (sy pendDisAngle) (int 2) :: Expr -- t2 is theta 2
          o2  = idx (sy pendDisAngle) (int 3) :: Expr -- o2 is omega 2
          g   = dbl 9.8 :: Expr -- should be sy gravitationalAccelConst but there is a bug
                        -- https://github.com/JacquesCarette/Drasil/issues/2998
          m1  = sy massObj_1 :: Expr
          m2  = sy massObj_2 :: Expr
          two = exactDbl 2 :: Expr
          l1  = sy lenRod_1 :: Expr
          l2  = sy lenRod_2 :: Expr
