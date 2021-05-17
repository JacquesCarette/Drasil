module Drasil.DblPendulum.DataDefs (dataDefs, positionIY, positionIX, angFrequencyDD,
         frequencyDD, periodSHMDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos, position,
      frequency, period, angularFrequency)
import Drasil.DblPendulum.Unitals (lenRod, initialPendAngle)
--import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Concepts (horizontal, vertical)


dataDefs :: [DataDefinition]
dataDefs = [positionIX, positionIY, frequencyDD, angFrequencyDD, periodSHMDD]


------------------------------------------------------
positionIX :: DataDefinition
positionIX = ddNoRefs positionIXQD Nothing "positionIX" [positionRef, figRef]

positionIXQD :: QDefinition
positionIXQD = mkQuantDef QP.ixPos positionIXEqn

positionIXEqn :: Expr
positionIXEqn = sy lenRod * sin (sy initialPendAngle)

figRef :: Sentence
figRef = ch QP.ixPos `S.sIs` S "shown in" +:+. makeRef2S figMotion

positionRef :: Sentence
positionRef = ch QP.ixPos `S.isThe` phrase horizontal +:+ phrase QP.position

------------------------------------------------------
positionIY :: DataDefinition
positionIY = ddNoRefs positionIYQD Nothing "positionIY" [positionReff, figReff]

positionIYQD :: QDefinition
positionIYQD = mkQuantDef QP.iyPos positionIYEqn

positionIYEqn :: Expr
positionIYEqn = negate (sy lenRod * cos (sy initialPendAngle))

figReff :: Sentence
figReff = ch QP.iyPos `S.sIs` S "shown in" +:+. makeRef2S figMotion

positionReff :: Sentence
positionReff = ch QP.iyPos `S.isThe` phrase vertical +:+ phrase QP.position

------------------------------------------------------

frequencyDD :: DataDefinition
frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

frequencyDDQD :: QDefinition
frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

frequencyDDEqn :: Expr
frequencyDDEqn = 1 / sy QP.period


frequencyRef :: Sentence
frequencyRef = ch QP.frequency `S.isThe` S "number of back and forth swings in one" +:+ phrase second

------------------------------------------------------

angFrequencyDD :: DataDefinition
angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef]

angFrequencyDDQD :: QDefinition
angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

angFrequencyDDEqn :: Expr
angFrequencyDDEqn = 2 * sy QM.pi_ / sy QP.period

angFrequencyRef :: Sentence
angFrequencyRef = ch QP.period `S.sIs` S "from" +:+ makeRef2S periodSHMDD

------------------------------------------------------

periodSHMDD :: DataDefinition
periodSHMDD = ddNoRefs periodSHMDDQD Nothing "periodSHMDD" [periodSHMRef]

periodSHMDDQD :: QDefinition
periodSHMDDQD = mkQuantDef QP.period periodSHMDDEqn

periodSHMDDEqn :: Expr
periodSHMDDEqn = 1 / sy QP.frequency

periodSHMRef :: Sentence
periodSHMRef = ch QP.period `S.sIs` S "from" +:+ makeRef2S frequencyDD
