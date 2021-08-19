module Drasil.SglPendulum.DataDefs (dataDefs, positionIY, positionIX, angFrequencyDD,
         frequencyDD, periodSHMDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.SglPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos,
      frequency, period, angularFrequency)
import Drasil.SglPendulum.Unitals (lenRod, initialPendAngle)
--import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Concepts (horizontalPos, verticalPos)


dataDefs :: [DataDefinition Expr]
dataDefs = [positionIX, positionIY, frequencyDD, angFrequencyDD, periodSHMDD]


------------------------------------------------------
positionIX :: DataDefinition Expr
positionIX = ddNoRefs positionIXQD Nothing "positionIX" [positionRef, figRef]

positionIXQD :: QDefinition Expr
positionIXQD = mkQuantDef QP.ixPos positionIXEqn

positionIXEqn :: Expr
positionIXEqn = sy lenRod `mulRe` sin (sy initialPendAngle)

figRef :: Sentence
figRef = ch QP.ixPos `S.is` S "shown in" +:+. refS figMotion

positionRef :: Sentence
positionRef = ch QP.ixPos `S.isThe` phrase horizontalPos

------------------------------------------------------
positionIY :: DataDefinition Expr
positionIY = ddNoRefs positionIYQD Nothing "positionIY" [positionReff, figReff]

positionIYQD :: QDefinition Expr
positionIYQD = mkQuantDef QP.iyPos positionIYEqn

positionIYEqn :: Expr
positionIYEqn = neg (sy lenRod `mulRe` cos (sy initialPendAngle))

figReff :: Sentence
figReff = ch QP.iyPos `S.is` S "shown in" +:+. refS figMotion

positionReff :: Sentence
positionReff = ch QP.iyPos `S.isThe` phrase verticalPos

------------------------------------------------------

frequencyDD :: DataDefinition Expr
frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

frequencyDDQD :: QDefinition Expr
frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

frequencyDDEqn :: Expr
frequencyDDEqn = recip_ $ sy QP.period


frequencyRef :: Sentence
frequencyRef = ch QP.frequency `S.isThe` S "number of back and forth swings in one" +:+ phrase second

------------------------------------------------------

angFrequencyDD :: DataDefinition Expr
angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef]

angFrequencyDDQD :: QDefinition Expr
angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

angFrequencyDDEqn :: Expr
angFrequencyDDEqn = exactDbl 2 `mulRe` sy QM.pi_ $/ sy QP.period

angFrequencyRef :: Sentence
angFrequencyRef = ch QP.period `S.is` S "from" +:+ refS periodSHMDD

------------------------------------------------------

periodSHMDD :: DataDefinition Expr
periodSHMDD = ddNoRefs periodSHMDDQD Nothing "periodSHMDD" [periodSHMRef]

periodSHMDDQD :: QDefinition Expr
periodSHMDDQD = mkQuantDef QP.period periodSHMDDEqn

periodSHMDDEqn :: Expr
periodSHMDDEqn = recip_ $ sy QP.frequency

periodSHMRef :: Sentence
periodSHMRef = ch QP.period `S.is` S "from" +:+ refS frequencyDD