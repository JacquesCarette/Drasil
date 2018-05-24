module Drasil.SSP.BasicExprs where

import Prelude hiding (sin, cos, tan)
import Language.Drasil
import Drasil.DocumentLanguage.RefHelpers

import Drasil.SSP.Unitals (baseAngle, genDisplace, rotatedDispl, dy_i,
  dx_i, inxi, index, nrmDispl, shrDispl, elmPrllDispl, elmNrmDispl,
  nrmStiffBase, shrStiffIntsl, genPressure, intShrForce, intNormForce,
  fy, fx, impLoadAngle, surfLoad, surfHydroForce, baseHydroForce,
  slcWght, inxiM1, surfAngle, earthqkLoadFctr, watrForceDif, midpntHght,
  baseWthX, sliceHght, watrForce, scalFunc, xi, normToShear, fs, shrResI,
  shearFNoIntsl, shrResI, mobShrI, nrmFSubWat, totNrmForce, baseLngth,
  shrStress, cohesion, fricAngle)
import Data.Drasil.Concepts.Documentation (element,
  system, value, variable, definition, model,
  assumption, property, method_)
import Drasil.SSP.Defs (slope, slice, intrslce, slpSrf)
import Data.Drasil.Concepts.PhysicalProperties (mass, len)
import Data.Drasil.Quantities.Physics (displacement, force)
import Data.Drasil.SentenceStructures (sAnd, getTandS,
  isThe, ofThe, foldlSent, acroGD, acroT)
import Data.Drasil.Concepts.SolidMechanics (normForce, shearForce)
import Data.Drasil.Quantities.SolidMechanics (nrmStrss)
import Data.Drasil.Concepts.Math (surface, angle,
  matrix, vector, perp, normal)
import Data.Drasil.Utils (getES)
import Drasil.SRS as SRS (physSyst, missingP)
import Drasil.SSP.Assumptions

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
