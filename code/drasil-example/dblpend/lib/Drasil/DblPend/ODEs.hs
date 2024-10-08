module Drasil.DblPend.ODEs (dblPenODEOpts, dblPenODEInfo) where

import Language.Drasil (ExprC(..), LiteralC(int, exactDbl, dbl), square)
import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)

import Data.Drasil.Quantities.Physics (time)

import Drasil.DblPend.Unitals(massObj_1, massObj_2, lenRod_1, lenRod_2, pendDisAngle)
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
  [ o1,
    neg g $* 
      (two $* m1 $+ m2) $* sin t1 $-
      (m2 $* g $* sin (t1 $- (two $* t2))) $-
      ((two $* sin (t1 $- t2 )) $* m2 $* 
      (square o2 $* l2 $+
      (square o1 $* l1 $* cos (t1 $- t2))))
      $/
      l1 $* (two $* m1 $+ m2 $-
      (m2 $* cos (two $* t1  $- (two $* t2)))),
    o2,
    two $* sin (t1 $- t2) $* 
      (square o1 $* l1 $* (m1 $+ m2 ) $+
      (g $* (m1 $+ m2 ) $* cos t1) $+
      (square o2 $* l2 $* m2 $* cos (t1 $- t2 )))
      $/
      l2 $* (two $* m1 $+ m2 $-
      (m2 $* cos (two $* t1  $- (two $* t2))))
    ]
  dblPenODEOpts
    where t1  = idx (sy pendDisAngle) (int 0) -- t1 is theta 1
          o1  = idx (sy pendDisAngle) (int 1) -- o1 is omega 1
          t2  = idx (sy pendDisAngle) (int 2) -- t2 is theta 2
          o2  = idx (sy pendDisAngle) (int 3) -- o2 is omega 2
          g   = dbl 9.8 -- should be sy gravitationalAccelConst but there is a bug
                        -- https://github.com/JacquesCarette/Drasil/issues/2998
          m1  = sy massObj_1
          m2  = sy massObj_2
          two = exactDbl 2
          l1  = sy lenRod_1
          l2  = sy lenRod_2
