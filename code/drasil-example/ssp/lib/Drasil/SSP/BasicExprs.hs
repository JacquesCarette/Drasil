module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.SSP.Unitals (baseAngle, baseWthX, earthqkLoadFctr, fs,
  impLoadAngle, intNormForce, inxi, inxiM1, midpntHght, mobShrC, shearFNoIntsl,
  shearRNoIntsl, shrResC, slcWght, sliceHght, sliceHghtW, surfAngle,
  surfHydroForce, surfLoad, watrForce)

eqlExpr :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExpr f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle))) `mulRe` f1_ (inxi baseAngle) $-
  (((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght) $- inxi intNormForce `add`
  inxiM1 intNormForce $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle))

eqlExprN :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprN f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle)) `mulRe` f1_ (inxi baseAngle)) `add`
  (((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi intNormForce `add`
  inxiM1 intNormForce $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `add`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle))

eqlExprSepG :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle)) `mulRe` f1_ (inxi baseAngle)) $-
  ((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle)) $-
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNSepG :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle))) `mulRe` f1_ (inxi baseAngle) `add`
  ((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle)) `add`
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNoKQ :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe` f1_ (inxi baseAngle) $-
  ((neg (inxi watrForce) `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` f2_ (inxi baseAngle)) $-
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNNoKQ :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe` f1_ (inxi baseAngle) `add`
  ((neg (inxi watrForce) `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` f2_ (inxi baseAngle)) `add`
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

sliceExpr :: (ExprC r, LiteralC r) => Integer -> r
sliceExpr n = idx (sy intNormForce) (int n) `mulRe` idx (sy shrResC) (int n) $=
  (idx (sy mobShrC) (int (n-1)) `mulRe` idx (sy intNormForce) (int (n-1)) `mulRe`
  idx (sy shrResC) (int (n-1)) `add` (sy fs `mulRe` idx (sy shearFNoIntsl) (int n)) $-
  idx (sy shearRNoIntsl) (int n))

momExpr :: (ExprC r, LiteralC r) => (r -> r -> r) -> r
momExpr _e_ = (neg (inxi intNormForce) `mulRe` (inxi sliceHght `add`(half (inxi baseWthX)
  `mulRe` tan (inxi baseAngle))) `add` (inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))) $-
  (inxi watrForce `mulRe` (oneThird `mulRe` inxi sliceHghtW `add` (half (inxi baseWthX) `mulRe`
  tan (inxi baseAngle)))) `add` (inxiM1 watrForce `mulRe` (oneThird `mulRe` inxiM1 sliceHghtW $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle))))) `_e_`
  (half (neg (sy earthqkLoadFctr) `mulRe` inxi slcWght `mulRe` inxi midpntHght) `add`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe` inxi midpntHght `add`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle) `mulRe` inxi midpntHght)))

momExprNoKQ :: (ExprC r, LiteralC r) => (r -> r -> r) -> r
momExprNoKQ _e_ = (neg (inxi intNormForce) `mulRe` (inxi sliceHght `add`(half (inxi baseWthX)
  `mulRe` tan (inxi baseAngle))) `add` (inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle)))) $-
  (inxi watrForce `mulRe` (oneThird `mulRe` inxi sliceHghtW `add` (half (inxi baseWthX) `mulRe`
  tan (inxi baseAngle)))) `add` (inxiM1 watrForce `mulRe` (oneThird `mulRe` inxiM1 sliceHghtW $-
  (half (inxi baseWthX) `mulRe` tan (inxi baseAngle))))) `_e_`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe` inxi midpntHght)
