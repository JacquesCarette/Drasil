module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.SSP.Unitals (baseAngle, baseWthX, earthqkLoadFctr, fs,
  impLoadAngle, intNormForce, inxi, inxiM1, midpntHght, mobShrC, shearFNoIntsl,
  shearRNoIntsl, shrResC, slcWght, sliceHght, sliceHghtW, surfAngle,
  surfHydroForce, surfLoad, watrForce)

eqlExpr :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExpr f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mul` cos (inxi impLoadAngle))) `mul` f1_ (inxi baseAngle) $-
  (((neg (sy earthqkLoadFctr) `mul` inxi slcWght) $- inxi intNormForce `add`
  inxiM1 intNormForce $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mul` sin (inxi impLoadAngle))) `mul` f2_ (inxi baseAngle))

eqlExprN :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprN f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mul` cos (inxi impLoadAngle)) `mul` f1_ (inxi baseAngle)) `add`
  (((neg (sy earthqkLoadFctr) `mul` inxi slcWght $- inxi intNormForce `add`
  inxiM1 intNormForce $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle))) `add`
  (inxi surfLoad `mul` sin (inxi impLoadAngle))) `mul` f2_ (inxi baseAngle))

eqlExprSepG :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mul` cos (inxi impLoadAngle)) `mul` f1_ (inxi baseAngle)) $-
  ((neg (sy earthqkLoadFctr) `mul` inxi slcWght $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mul` sin (inxi impLoadAngle))) `mul` f2_ (inxi baseAngle)) $-
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mul` f2_ (inxi baseAngle))

eqlExprNSepG :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle)) `add`
  (inxi surfLoad `mul` cos (inxi impLoadAngle))) `mul` f1_ (inxi baseAngle) `add`
  ((neg (sy earthqkLoadFctr) `mul` inxi slcWght $- inxi watrForce `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle)) `add`
  (inxi surfLoad `mul` sin (inxi impLoadAngle))) `mul` f2_ (inxi baseAngle)) `add`
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mul` f2_ (inxi baseAngle))

eqlExprNoKQ :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle))) `mul` f1_ (inxi baseAngle) $-
  ((neg (inxi watrForce) `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle))) `mul` f2_ (inxi baseAngle)) $-
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mul` f2_ (inxi baseAngle))

eqlExprNNoKQ :: (ExprC r, LiteralC r) => (r -> r) -> (r -> r) -> (r -> r -> r) -> r
eqlExprNNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mul` cos (inxi surfAngle))) `mul` f1_ (inxi baseAngle) `add`
  ((neg (inxi watrForce) `add` inxiM1 watrForce `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle))) `mul` f2_ (inxi baseAngle)) `add`
  ((neg (inxi intNormForce) `add` inxiM1 intNormForce) `mul` f2_ (inxi baseAngle))

sliceExpr :: (ExprC r, LiteralC r) => Integer -> r
sliceExpr n = idx (sy intNormForce) (int n) `mul` idx (sy shrResC) (int n) $=
  (idx (sy mobShrC) (int (n-1)) `mul` idx (sy intNormForce) (int (n-1)) `mul`
  idx (sy shrResC) (int (n-1)) `add` (sy fs `mul` idx (sy shearFNoIntsl) (int n)) $-
  idx (sy shearRNoIntsl) (int n))

momExpr :: (ExprC r, LiteralC r) => (r -> r -> r) -> r
momExpr _e_ = (neg (inxi intNormForce) `mul` (inxi sliceHght `add`(half (inxi baseWthX)
  `mul` tan (inxi baseAngle))) `add` (inxiM1 intNormForce `mul` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mul` tan (inxi baseAngle)))) $-
  (inxi watrForce `mul` (oneThird `mul` inxi sliceHghtW `add` (half (inxi baseWthX) `mul`
  tan (inxi baseAngle)))) `add` (inxiM1 watrForce `mul` (oneThird `mul` inxiM1 sliceHghtW $-
  (half (inxi baseWthX) `mul` tan (inxi baseAngle))))) `_e_`
  (half (neg (sy earthqkLoadFctr) `mul` inxi slcWght `mul` inxi midpntHght) `add`
  (inxi surfHydroForce `mul` sin (inxi surfAngle) `mul` inxi midpntHght `add`
  (inxi surfLoad `mul` sin (inxi impLoadAngle) `mul` inxi midpntHght)))

momExprNoKQ :: (ExprC r, LiteralC r) => (r -> r -> r) -> r
momExprNoKQ _e_ = (neg (inxi intNormForce) `mul` (inxi sliceHght `add`(half (inxi baseWthX)
  `mul` tan (inxi baseAngle))) `add` (inxiM1 intNormForce `mul` (inxiM1 sliceHght $-
  (half (inxi baseWthX) `mul` tan (inxi baseAngle)))) $-
  (inxi watrForce `mul` (oneThird `mul` inxi sliceHghtW `add` (half (inxi baseWthX) `mul`
  tan (inxi baseAngle)))) `add` (inxiM1 watrForce `mul` (oneThird `mul` inxiM1 sliceHghtW $-
  (half (inxi baseWthX) `mul` tan (inxi baseAngle))))) `_e_`
  (inxi surfHydroForce `mul` sin (inxi surfAngle) `mul` inxi midpntHght)
