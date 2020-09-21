module Drasil.DblPendulum.DataDefs (dataDefs, positionIY, positionIX) where

import Prelude hiding (sin, cos)
import Language.Drasil
import Utils.Drasil

import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos, position)
import Drasil.DblPendulum.Unitals (lenRod, pendAngle)


dataDefs :: [DataDefinition]
dataDefs = [positionIX, positionIY]


----------
positionIX :: DataDefinition
positionIX = ddNoRefs positionIXQD Nothing "positionIX" [positionRef, figRef]

positionIXQD :: QDefinition
positionIXQD = mkQuantDef QP.ixPos positionIXEqn

positionIXEqn :: Expr
positionIXEqn = sy lenRod * sin (sy pendAngle)

figRef :: Sentence
figRef = ch QP.ixPos `sIs` S "shown in" +:+. makeRef2S figMotion

positionRef :: Sentence
positionRef = ch QP.ixPos `isThe` S "horizontal" +:+ phrase QP.position

--------------------------------------------
positionIY :: DataDefinition
positionIY = ddNoRefs positionIYQD Nothing "positionIY" [positionReff, figReff]

positionIYQD :: QDefinition
positionIYQD = mkQuantDef QP.iyPos positionIYEqn

positionIYEqn :: Expr
positionIYEqn = sy lenRod * cos (sy pendAngle)

figReff :: Sentence
figReff = ch QP.iyPos `sIs` S "shown in" +:+. makeRef2S figMotion

positionReff :: Sentence
positionReff = ch QP.iyPos `isThe` S "vertical" +:+ phrase QP.position
