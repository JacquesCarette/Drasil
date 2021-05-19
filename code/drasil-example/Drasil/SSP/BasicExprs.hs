module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.SSP.Unitals (baseAngle, baseWthX, earthqkLoadFctr, fs,
  impLoadAngle, intNormForce, inxi, inxiM1, midpntHght, mobShrC, shearFNoIntsl,
  shearRNoIntsl, shrResC, slcWght, sliceHght, sliceHghtW, surfAngle,
  surfHydroForce, surfLoad, watrForce)

eqlExpr :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExpr f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle))) `mulRe` f1_ (inxi baseAngle) $-
  (((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght) $- inxi intNormForce `addRe`
  inxiM1 intNormForce $- inxi watrForce `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle))

eqlExprN :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprN f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle)) `mulRe` f1_ (inxi baseAngle)) `addRe`
  (((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi intNormForce `addRe`
  inxiM1 intNormForce $- inxi watrForce `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `addRe`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle))

eqlExprSepG :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle)) `mulRe` f1_ (inxi baseAngle)) $-
  ((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi watrForce `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle)) $- 
  ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNSepG :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` cos (inxi impLoadAngle))) `mulRe` f1_ (inxi baseAngle) `addRe`
  ((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght $- inxi watrForce `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle)) `addRe`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle))) `mulRe` f2_ (inxi baseAngle)) `addRe`
  ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNoKQ :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe` f1_ (inxi baseAngle) $-
  ((neg (inxi watrForce) `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` f2_ (inxi baseAngle)) $- 
  ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

eqlExprNNoKQ :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce `mulRe` cos (inxi surfAngle))) `mulRe` f1_ (inxi baseAngle) `addRe`
  ((neg (inxi watrForce) `addRe` inxiM1 watrForce `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle))) `mulRe` f2_ (inxi baseAngle)) `addRe`
  ((neg (inxi intNormForce) `addRe` inxiM1 intNormForce) `mulRe` f2_ (inxi baseAngle))

sliceExpr :: Integer -> Expr
sliceExpr n = idx (sy intNormForce) (int n) `mulRe` idx (sy shrResC) (int n) $=
  (idx (sy mobShrC) (int (n-1)) `mulRe` idx (sy intNormForce) (int (n-1)) `mulRe`
  idx (sy shrResC) (int (n-1)) `addRe` (sy fs `mulRe` idx (sy shearFNoIntsl) (int n)) $-
  idx (sy shearRNoIntsl) (int n))

momExpr :: (Expr -> Expr -> Expr) -> Expr
momExpr _e_ = (neg (inxi intNormForce) `mulRe` (inxi sliceHght `addRe`((inxi baseWthX $/ dbl 2)
  `mulRe`  tan (inxi baseAngle))) `addRe` (inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  ((inxi baseWthX $/ dbl 2) `mulRe` tan (inxi baseAngle)))) $-
  (inxi watrForce `mulRe` ((int 1 $/ int 3) `mulRe` inxi sliceHghtW `addRe` ((inxi baseWthX $/ dbl 2) `mulRe`
  tan (inxi baseAngle)))) `addRe` (inxiM1 watrForce `mulRe` ((int 1 $/ int 3) `mulRe` inxiM1 sliceHghtW $-
  ((inxi baseWthX $/ dbl 2) `mulRe` tan (inxi baseAngle))))) `_e_`
  ((neg (sy earthqkLoadFctr) `mulRe` inxi slcWght `mulRe` inxi midpntHght $/ dbl 2) `addRe`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe` inxi midpntHght `addRe`
  (inxi surfLoad `mulRe` sin (inxi impLoadAngle) `mulRe` inxi midpntHght)))

momExprNoKQ :: (Expr -> Expr -> Expr) -> Expr
momExprNoKQ _e_ = (neg (inxi intNormForce) `mulRe` (inxi sliceHght `addRe`((inxi baseWthX $/ dbl 2)
  `mulRe`  tan (inxi baseAngle))) `addRe` (inxiM1 intNormForce `mulRe` (inxiM1 sliceHght $-
  ((inxi baseWthX $/ dbl 2) `mulRe` tan (inxi baseAngle)))) $-
  (inxi watrForce `mulRe` ((int 1 $/ int 3) `mulRe` inxi sliceHghtW `addRe` ((inxi baseWthX $/ dbl 2) `mulRe`
  tan (inxi baseAngle)))) `addRe` (inxiM1 watrForce `mulRe` ((int 1 $/ int 3) `mulRe` inxiM1 sliceHghtW $-
  ((inxi baseWthX $/ dbl 2) `mulRe` tan (inxi baseAngle))))) `_e_`
  (inxi surfHydroForce `mulRe` sin (inxi surfAngle) `mulRe` inxi midpntHght)
