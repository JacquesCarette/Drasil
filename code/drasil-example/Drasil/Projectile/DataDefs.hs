module Drasil.Projectile.DataDefs (dataDefns, velY) where

import Prelude hiding (sin, cos)
import Language.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)

import Data.Drasil.Quantities.Physics (velocity)

import Drasil.Projectile.Unitals (projAngle, vx, vy)

dataDefns :: [DataDefinition]
dataDefns = [velX, velY]

----------
velX :: DataDefinition
velX = ddNoRefs velXQD [{-Derivation-}] "velX" [{-Notes-}]

velXQD :: QDefinition
velXQD = mkQuantDef vx velXEqn

velXEqn :: Expr
velXEqn = (sy velocity) * cos (sy projAngle)

----------
velY :: DataDefinition
velY = ddNoRefs velYQD [{-Derivation-}] "velY" [{-Notes-}]

velYQD :: QDefinition
velYQD = mkQuantDef vy velYEqn

velYEqn :: Expr
velYEqn = (sy velocity) * sin (sy projAngle)
