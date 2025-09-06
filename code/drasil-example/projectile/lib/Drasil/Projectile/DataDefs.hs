module Drasil.Projectile.DataDefs (dataDefs, speedIX, speedIY, speedDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Theory.Drasil (DataDefinition, ddENoRefs)

import Data.Drasil.Quantities.Physics (iSpeed, ixVel, iyVel, speed)

import Drasil.Projectile.LabelledContent (figLaunch)
import Drasil.Projectile.Unitals (launAngle)

dataDefs :: [DataDefinition]
dataDefs = [speedDD, speedIX, speedIY]

----------
speedDD :: DataDefinition 
speedDD = ddENoRefs speedQD Nothing "vecMag" [speedNote]

speedQD :: SimpleQDef
speedQD = mkQuantDef speed $ sqrt (square (sy ixVel) $+ square (sy iyVel))

speedNote :: Sentence
speedNote = S "The magnitude of velocity computed from its components"

speedIX, speedIY :: DataDefinition
speedIX = ddENoRefs speedIXQD Nothing "speedIX" [speedRef, figRef]
speedIY = ddENoRefs speedIYQD Nothing "speedIY" [speedRef, figRef]

speedIXQD, speedIYQD :: SimpleQDef
speedIXQD = mkQuantDef ixVel $ sy iSpeed $* cos (sy launAngle)
speedIYQD = mkQuantDef iyVel $ sy iSpeed $* sin (sy launAngle)

----------
speedRef :: Sentence
speedRef = ch iSpeed `S.is` S "from" +:+. refS speedDD

figRef :: Sentence
figRef = ch launAngle `S.is` S "shown in" +:+. refS figLaunch
