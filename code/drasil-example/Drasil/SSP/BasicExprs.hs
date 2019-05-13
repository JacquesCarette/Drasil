module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.SSP.Unitals (baseAngle, baseWthX, earthqkLoadFctr, fs,
  impLoadAngle, intNormForce, inxi, inxiM1, midpntHght, mobShrC, shearFNoIntsl,
  shearRNoIntsl, shrResC, slcWght, sliceHght, sliceHghtW, surfAngle, 
  surfHydroForce, surfLoad, watrForce)

eqlExpr :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExpr f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) -
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi intNormForce) +
  (inxiM1 intNormForce) - (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle))

eqlExprN :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprN f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi intNormForce) +
  (inxiM1 intNormForce) - (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle))

eqlExprSepG :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) -
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle)) - 
  (- (inxi intNormForce) + (inxiM1 intNormForce)) * (f2_ (inxi baseAngle))

eqlExprNSepG :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNSepG f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle)) +
  (- (inxi intNormForce) + (inxiM1 intNormForce)) * (f2_ (inxi baseAngle))

eqlExprNoKQ :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle))) * (f1_ (inxi baseAngle)) -
  (- (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle)) * (f2_ (inxi baseAngle)) - 
  (- (inxi intNormForce) + (inxiM1 intNormForce)) * (f2_ (inxi baseAngle))

eqlExprNNoKQ :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExprNNoKQ f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle))) * (f1_ (inxi baseAngle)) +
  (- (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle)) * (f2_ (inxi baseAngle)) +
  (- (inxi intNormForce) + (inxiM1 intNormForce)) * (f2_ (inxi baseAngle))

sliceExpr :: Integer -> Expr
sliceExpr n = idx (sy intNormForce) (int n) * idx (sy shrResC) (int n) $= 
  idx (sy mobShrC) (int (n-1)) * idx (sy intNormForce) (int (n-1)) * 
  idx (sy shrResC) (int (n-1)) + sy fs * idx (sy shearFNoIntsl) (int n) - 
  idx (sy shearRNoIntsl) (int n)

momExpr :: (Expr -> Expr -> Expr) -> Expr
momExpr _e_ = (negate (inxi intNormForce) * (inxi sliceHght +
  inxi baseWthX / 2 *  tan (inxi baseAngle)) + inxiM1 intNormForce *
  (inxiM1 sliceHght - inxi baseWthX / 2 * tan (inxi baseAngle)) -
  inxi watrForce * (inxi sliceHghtW + inxi baseWthX / 2 *
  tan (inxi baseAngle)) + inxiM1 watrForce * (inxiM1 sliceHghtW -
  inxi baseWthX / 2 * tan (inxi baseAngle))) `_e_`
  (sy earthqkLoadFctr * inxi slcWght * inxi midpntHght / 2 +
  inxi surfHydroForce * sin (inxi surfAngle) * inxi midpntHght +
  inxi surfLoad * sin (inxi impLoadAngle) * inxi midpntHght)

momExprNoKQ :: (Expr -> Expr -> Expr) -> Expr
momExprNoKQ _e_ = (negate (inxi intNormForce) * (inxi sliceHght -
  inxi baseWthX / 2 *  tan (inxi baseAngle)) + inxiM1 intNormForce *
  (inxiM1 sliceHght + inxi baseWthX / 2 * tan (inxi baseAngle)) -
  inxi watrForce * (inxi sliceHghtW - inxi baseWthX / 2 *
  tan (inxi baseAngle)) + inxiM1 watrForce * (inxiM1 sliceHghtW +
  inxi baseWthX / 2 * tan (inxi baseAngle))) `_e_`
  (inxi surfHydroForce * sin (inxi surfAngle) * inxi midpntHght)
