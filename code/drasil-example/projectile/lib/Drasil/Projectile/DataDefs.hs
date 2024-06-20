module Drasil.Projectile.DataDefs (dataDefs, speedIX, speedIY) where

import Prelude hiding (sin, cos)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Theory.Drasil (DataDefinition, ddENoRefs)

import Data.Drasil.Quantities.Physics (iSpeed, ixVel, iyVel)
import Data.Drasil.Theories.Physics (vecMag)

import Drasil.Projectile.Figures (figLaunch)
import Drasil.Projectile.Unitals (launAngle)

dataDefs :: [DataDefinition]
dataDefs = [vecMag, speedIX, speedIY]

----------
speedIX, speedIY :: DataDefinition
speedIX = ddENoRefs speedIXQD Nothing "speedIX" [speedRef, figRef]
speedIY = ddENoRefs speedIYQD Nothing "speedIY" [speedRef, figRef]

speedIXQD, speedIYQD :: SimpleQDef
speedIXQD = mkQuantDef ixVel $ sy iSpeed $* cos (sy launAngle)
speedIYQD = mkQuantDef iyVel $ sy iSpeed $* sin (sy launAngle)

----------
speedRef :: Sentence
speedRef = ch iSpeed `S.is` S "from" +:+. refS vecMag

figRef :: Sentence
figRef = ch launAngle `S.is` S "shown in" +:+. refS figLaunch
