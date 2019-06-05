module Drasil.Projectile.DataDefs (dataDefns) where

import Prelude hiding (sin, cos)
import Language.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)

import Data.Drasil.Quantities.Physics (iVel, ixVel, iyVel)

import Drasil.Projectile.Unitals (launAngle)

dataDefns :: [DataDefinition]
dataDefns = [speedIX, speedIY]

----------
speedIX :: DataDefinition
speedIX = ddNoRefs speedIXQD [{-Derivation-}] "speedIX" [{-Notes-}]

speedIXQD :: QDefinition
speedIXQD = mkQuantDef ixVel speedIXEqn

speedIXEqn :: Expr
speedIXEqn = UnaryOp Abs (sy iVel) * cos (sy launAngle)

----------
speedIY :: DataDefinition
speedIY = ddNoRefs speedIYQD [{-Derivation-}] "speedIY" [{-Notes-}]

speedIYQD :: QDefinition
speedIYQD = mkQuantDef iyVel speedIYEqn

speedIYEqn :: Expr
speedIYEqn = UnaryOp Abs (sy iVel) * sin (sy launAngle)
