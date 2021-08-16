module Drasil.Projectile.DataDefs (dataDefs, speedIX, speedIY) where

import Prelude hiding (sin, cos)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S

import Theory.Drasil (DataDefinition, ddNoRefs)

import Data.Drasil.Quantities.Physics (iSpeed, ixVel, iyVel)
import Data.Drasil.Theories.Physics (vecMag)

import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.Unitals (launAngle)

dataDefs :: [DataDefinition Expr]
dataDefs = [vecMag, speedIX, speedIY]

----------
speedIX, speedIY :: DataDefinition Expr
speedIX = ddNoRefs speedIXQD Nothing "speedIX" [speedRef, figRef]
speedIY = ddNoRefs speedIYQD Nothing "speedIY" [speedRef, figRef]

speedIXQD, speedIYQD :: QDefinition Expr
speedIXQD = mkQuantDef ixVel $ sy iSpeed `mulRe` cos (sy launAngle)
speedIYQD = mkQuantDef iyVel $ sy iSpeed `mulRe` sin (sy launAngle)

----------
speedRef :: Sentence
speedRef = ch iSpeed `S.is` S "from" +:+. refS vecMag

figRef :: Sentence
figRef = ch launAngle `S.is` S "shown in" +:+. refS figLaunch
