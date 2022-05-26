module Drasil.DblPendulum.ODEs (dblPenODEOpts, dblPenODEInfo) where

import Language.Drasil (recip_, ExprC(..), LiteralC(int, exactDbl, dbl), square)
import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)

import Data.Drasil.Quantities.Physics (time, gravitationalAccelConst)

import Drasil.DblPendulum.Unitals(massObj_1, massObj_2, lenRod_1, lenRod_2, pendDisAngle)
import Prelude hiding (sin, cos)

dblPenODEOpts :: ODEOptions
dblPenODEOpts = odeOptions RK45 (dbl 0.001) (dbl 0.001) (dbl 0.01)

dblPenODEInfo :: ODEInfo
dblPenODEInfo = odeInfo
  (quantvar time)
  (quantvar pendDisAngle)
  [quantvar massObj_1,
    quantvar massObj_2, quantvar lenRod_1, quantvar lenRod_2]
  (exactDbl 0)
  (exactDbl 30) -- final time
  [dbl 0.5, exactDbl 0, dbl 0.25, exactDbl 0]
  [o1,
    neg g `mulRe`
        (two `mulRe` m1 `addRe` m2) `mulRe` sin t1 $-
        (m2 `mulRe` g `mulRe`
        sin (t1 $- (two `mulRe` t2))) $-
        ((two `mulRe` sin (t1 $- t2 )) `mulRe` m2 `mulRe`
        (
            square o2 `mulRe` l2 `addRe`
            (square o1 `mulRe` l1 `mulRe` cos (t1 $- t2))
        ))
        $/
        l1 `mulRe`
        (
            two `mulRe` m1 `addRe` m2 $-
            (m2 `mulRe`
            cos (two `mulRe` t1  $- (two `mulRe` t2)))
        ),
    two `mulRe` sin (t1 $- t2) `mulRe`
        (
            square o1 `mulRe` l1 `mulRe` (m1 `addRe` m2 ) `addRe`
            (g `mulRe` (m1 `addRe` m2 ) `mulRe` cos t1) `addRe`
            (square o2 `mulRe` l2 `mulRe` m2 `mulRe`
            cos (t1 $- t2 ))
        )
        $/
        l2 `mulRe`
        (
            two `mulRe` m1 `addRe` m2 $-
            (m2 `mulRe`
            cos (two `mulRe` t1  $- (two `mulRe` t2)))
        ),
    o2]
  dblPenODEOpts
    where t1 = idx (sy pendDisAngle) (int 0) -- t1 is theta 1
          o1 = idx (sy pendDisAngle) (int 1) -- o1 is omega 1
          t2 = idx (sy pendDisAngle) (int 2) -- t2 is theta 2
          o2 = idx (sy pendDisAngle) (int 3) -- o2 is omega 2
          g = sy gravitationalAccelConst
          m1 = sy massObj_1
          m2 = sy massObj_2
          two = exactDbl 2
          l1 = sy lenRod_1
          l2 = sy lenRod_2
