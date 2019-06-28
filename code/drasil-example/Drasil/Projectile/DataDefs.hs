module Drasil.Projectile.DataDefs (dataDefns, speedIX, speedIY) where

import Prelude hiding (sin, cos)
import Language.Drasil
import Utils.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)

import Data.Drasil.Quantities.Physics (speed, iSpeed, ixVel, iyVel, velocity)

import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.Unitals (launAngle)

dataDefns :: [DataDefinition]
dataDefns = [vecMag, speedIX, speedIY]

----------
vecMag :: DataDefinition
vecMag = ddNoRefs vecMagQD [{-Derivation-}] "vecMag" [magNote]

vecMagQD :: QDefinition
vecMagQD = mkQuantDef speed vecMagEqn

vecMagEqn :: Expr
vecMagEqn = UnaryOp Abs (sy velocity)

----------
speedIX :: DataDefinition
speedIX = ddNoRefs speedIXQD [{-Derivation-}] "speedIX" [speedRef, figRef]

speedIXQD :: QDefinition
speedIXQD = mkQuantDef ixVel speedIXEqn

speedIXEqn :: Expr
speedIXEqn = sy iSpeed * cos (sy launAngle)

----------
speedIY :: DataDefinition
speedIY = ddNoRefs speedIYQD [{-Derivation-}] "speedIY" [speedRef, figRef]

speedIYQD :: QDefinition
speedIYQD = mkQuantDef iyVel speedIYEqn

speedIYEqn :: Expr
speedIYEqn = sy iSpeed * sin (sy launAngle)

----------
magNote :: Sentence
magNote = foldlSent [S "For a given", phrase velocity, S "vector", ch velocity `sC`
  S "the magnitude of the vector", sParen (E $ UnaryOp Abs (sy velocity)) `isThe`
  S "scalar called", phrase speed]

speedRef :: Sentence
speedRef = ch iSpeed `sIs` S "from" +:+. makeRef2S vecMag

figRef :: Sentence
figRef = ch launAngle `sIs` S "shown in" +:+. makeRef2S figLaunch
