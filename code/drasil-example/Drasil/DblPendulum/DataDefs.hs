module Drasil.DblPendulum.DataDefs (dataDefs, positionIY, positionIX) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import Utils.Drasil
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs, mkQuantDef)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos, position,
      frequency, period, angularFrequency, gravitationalAccel)
import Drasil.DblPendulum.Unitals (lenRod, pendDisplacementAngle, initialPendAngle)
import Data.Drasil.Concepts.Physics (pendulum)


dataDefs :: [DataDefinition]
dataDefs = [positionIX, positionIY, frequencyDD, angFrequencyDD]


----------
positionIX :: DataDefinition
positionIX = ddNoRefs positionIXQD Nothing "positionIX" [positionRef, figRef]

positionIXQD :: QDefinition
positionIXQD = mkQuantDef QP.ixPos positionIXEqn

positionIXEqn :: Expr
positionIXEqn = sy lenRod * sin (sy initialPendAngle)

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
positionIYEqn = sy lenRod * cos (sy initialPendAngle)

figReff :: Sentence
figReff = ch QP.iyPos `sIs` S "shown in" +:+. makeRef2S figMotion

positionReff :: Sentence
positionReff = ch QP.iyPos `isThe` S "vertical" +:+ phrase QP.position

----------------------------------------------

frequencyDD :: DataDefinition
frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

frequencyDDQD :: QDefinition
frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

frequencyDDEqn :: Expr
frequencyDDEqn = 1 / sy QP.period


frequencyRef :: Sentence
frequencyRef = ch QP.frequency `isThe` S "number of back and forth swings in one" +:+ phrase second

-----------------------------------------------------

angFrequencyDD :: DataDefinition
angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef, angFrequencyNote]

angFrequencyDDQD :: QDefinition
angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

angFrequencyDDEqn :: Expr
angFrequencyDDEqn = sqrt (sy QP.gravitationalAccel / sy lenRod)


angFrequencyRef :: Sentence
angFrequencyRef = ch QP.angularFrequency `isThe` S "natural" +:+
     phrase QP.frequency +:+ S "of" +:+ phrase pendulum +:+ S "motion"

angFrequencyNote :: Sentence
angFrequencyNote = S "The" +:+ phrase QP.angularFrequency `sIs`
     S "connected with" +:+ makeRef2S frequencyDD
------------------------------------------------------

-- angFrequencyDD :: DataDefinition
-- angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [frequencyRef]

-- angFrequencyDDQD :: QDefinition
-- angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

-- angFrequencyDDEqn :: Expr
-- angFrequencyDDEqn = sqrt sy gravitationalAccel / sy lenRod


-- angFrequencyRef :: Sentence
-- angFrequencyRef = ch QP.angularFrequency `isThe` S "natural" +:+ phrase frequency +:+ S "of" +:+ phrase pendulum +:+ phrase motion


