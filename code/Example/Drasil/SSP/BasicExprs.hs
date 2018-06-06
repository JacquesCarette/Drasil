module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil

import Drasil.SSP.Unitals (baseAngle, dy_i, dx_i, inxi,
  intNormForce, impLoadAngle, surfLoad, surfHydroForce,
  slcWght, inxiM1, surfAngle, earthqkLoadFctr, midpntHght,
  baseWthX, sliceHght, watrForce)

eqlExpr :: (Expr -> Expr) -> (Expr -> Expr) -> (Expr -> Expr -> Expr) -> Expr
eqlExpr f1_ f2_ _e_ = (inxi slcWght `_e_`
  (inxi surfHydroForce * cos (inxi surfAngle)) +
  (inxi surfLoad) * (cos (inxi impLoadAngle))) * (f1_ (inxi baseAngle)) +
  (negate (sy earthqkLoadFctr) * (inxi slcWght) - (inxi intNormForce) +
  (inxiM1 intNormForce) - (inxi watrForce) + (inxiM1 watrForce) +
  (inxi surfHydroForce) * sin (inxi surfAngle) +
  (inxi surfLoad) * (sin (inxi impLoadAngle))) * (f2_ (inxi baseAngle))

displMtx :: Expr
displMtx = vec2D (inxi dx_i) (inxi dy_i)

rotMtx :: Expr
rotMtx = m2x2
  (cos(inxi baseAngle))       (sin(inxi baseAngle))
  (negate $ sin(inxi baseAngle)) (cos(inxi baseAngle))

momExpr :: (Expr -> Expr -> Expr) -> Expr
momExpr _e_ = (negate (inxi intNormForce) * (inxi sliceHght -
  inxi baseWthX / 2 *  tan (inxi baseAngle)) + inxiM1 intNormForce *
  (inxiM1 sliceHght - inxi baseWthX / 2 * tan (inxi baseAngle)) -
  inxi watrForce * (inxi sliceHght - inxi baseWthX / 2 *
  tan (inxi baseAngle)) + inxiM1 watrForce * (inxiM1 sliceHght -
  inxi baseWthX / 2 * tan (inxi baseAngle))) `_e_`
  (sy earthqkLoadFctr * inxi slcWght * inxi midpntHght / 2 -
  inxi surfHydroForce * sin (inxi surfAngle) * inxi midpntHght -
  inxi surfLoad * sin (inxi impLoadAngle) * inxi midpntHght)
