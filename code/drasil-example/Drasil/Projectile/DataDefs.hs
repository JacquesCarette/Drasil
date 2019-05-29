module Drasil.Projectile.DataDefs (dataDefns, speedY) where

import Prelude hiding (sin, cos)
import Language.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)

import Data.Drasil.Quantities.Physics (speed, xSpeed, ySpeed)

import Drasil.Projectile.Unitals (launAngle)

dataDefns :: [DataDefinition]
dataDefns = [speedX, speedY]

----------
speedX :: DataDefinition
speedX = ddNoRefs speedXQD [{-Derivation-}] "speedX" [{-Notes-}]

speedXQD :: QDefinition
speedXQD = mkQuantDef xSpeed speedXEqn

speedXEqn :: Expr
speedXEqn = sy speed * cos (sy launAngle)

----------
speedY :: DataDefinition
speedY = ddNoRefs speedYQD [{-Derivation-}] "speedY" [{-Notes-}]

speedYQD :: QDefinition
speedYQD = mkQuantDef ySpeed speedYEqn

speedYEqn :: Expr
speedYEqn = sy speed * sin (sy launAngle)
