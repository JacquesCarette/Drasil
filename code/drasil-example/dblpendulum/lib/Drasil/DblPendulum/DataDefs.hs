module Drasil.DblPendulum.DataDefs (dataDefs, positionGDD, positionIY_1DD, positionIX_1DD, positionIX_2DD, positionIY_2DD, 
      angFrequencyDD, frequencyDD, periodSHMDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos,
      frequency, period, angularFrequency)
import Drasil.DblPendulum.Unitals (lenRod, pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2, ixPos_1, ixPos_2, iyPos_1, iyPos_2)
--import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Concepts (horizontalPos, verticalPos)
import Data.Drasil.Quantities.Physics (velocity, position, time)


dataDefs :: [DataDefinition]
dataDefs = [positionGDD, positionIX_1DD, positionIY_1DD, positionIX_2DD, positionIY_2DD,
  frequencyDD, angFrequencyDD, periodSHMDD]

------------------------
-- Position in General--
------------------------
positionGDD :: DataDefinition
positionGDD = ddNoRefs positionGQD Nothing "positionGDD" []

positionGQD :: QDefinition
positionGQD = mkQuantDef velocity positionGEqn

positionGEqn :: Expr
positionGEqn = deriv (sy position) time

-----------------------------------------------
-- Position in X Dirction in the First Object--
-----------------------------------------------
positionIX_1DD :: DataDefinition
positionIX_1DD = ddNoRefs positionIX_1QD Nothing "positionIX1DD" [positionIX_1Ref, positionIX_1FigRef]

positionIX_1QD :: QDefinition
positionIX_1QD = mkQuantDef ixPos_1 positionIX_1Eqn

positionIX_1Eqn :: Expr
positionIX_1Eqn = sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)

positionIX_1FigRef :: Sentence
positionIX_1FigRef = ch ixPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIX_1Ref :: Sentence
positionIX_1Ref = ch ixPos_1 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the First Object--
-----------------------------------------------
positionIY_1DD :: DataDefinition
positionIY_1DD = ddNoRefs positionIY_1QD Nothing "positionIY1DD" [positionIY_1Ref, positionIY_1FigRef]

positionIY_1QD :: QDefinition
positionIY_1QD = mkQuantDef iyPos_1 positionIY_1Eqn

positionIY_1Eqn :: Expr
positionIY_1Eqn = neg (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))

positionIY_1FigRef :: Sentence
positionIY_1FigRef = ch iyPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIY_1Ref :: Sentence
positionIY_1Ref = ch iyPos_1 `S.isThe` phrase verticalPos

-----------------------------------------------
-- Position in X Dirction in the Second Object--
-----------------------------------------------
positionIX_2DD :: DataDefinition
positionIX_2DD = ddNoRefs positionIX_2QD Nothing "positionIX2DD" [positionIX_2Ref, positionIX_2FigRef]

positionIX_2QD :: QDefinition
positionIX_2QD = mkQuantDef ixPos_2 positionIX_2Eqn

positionIX_2Eqn :: Expr
positionIX_2Eqn = sy positionIX_1QD `addRe` (sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

positionIX_2FigRef :: Sentence
positionIX_2FigRef = ch ixPos_2 `S.is` S "shown in" +:+. refS figMotion

positionIX_2Ref :: Sentence
positionIX_2Ref = ch ixPos_2 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the Second Object--
-----------------------------------------------
positionIY_2DD :: DataDefinition
positionIY_2DD = ddNoRefs positionIY_2QD Nothing "positionIY2DD" [positionIY_2Ref, positionIY_2FigRef]

positionIY_2QD :: QDefinition
positionIY_2QD = mkQuantDef iyPos_2 positionIY_2Eqn

positionIY_2Eqn :: Expr
positionIY_2Eqn = sy positionIY_1QD `addRe` neg (sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))

positionIY_2FigRef :: Sentence
positionIY_2FigRef = ch iyPos_2 `S.is` S "shown in" +:+. refS figMotion

positionIY_2Ref :: Sentence
positionIY_2Ref = ch iyPos_2 `S.isThe` phrase verticalPos

------------------------------------------------------
frequencyDD :: DataDefinition
frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

frequencyDDQD :: QDefinition
frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

frequencyDDEqn :: Expr
frequencyDDEqn = recip_ $ sy QP.period

frequencyRef :: Sentence
frequencyRef = ch QP.frequency `S.isThe` S "number of back and forth swings in one" +:+ phrase second

------------------------------------------------------

angFrequencyDD :: DataDefinition
angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef]

angFrequencyDDQD :: QDefinition
angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

angFrequencyDDEqn :: Expr
angFrequencyDDEqn = exactDbl 2 `mulRe` sy QM.pi_ $/ sy QP.period

angFrequencyRef :: Sentence
angFrequencyRef = ch QP.period `S.is` S "from" +:+ refS periodSHMDD

------------------------------------------------------

periodSHMDD :: DataDefinition
periodSHMDD = ddNoRefs periodSHMDDQD Nothing "periodSHMDD" [periodSHMRef]

periodSHMDDQD :: QDefinition
periodSHMDDQD = mkQuantDef QP.period periodSHMDDEqn

periodSHMDDEqn :: Expr
periodSHMDDEqn = recip_ $ sy QP.frequency

periodSHMRef :: Sentence
periodSHMRef = ch QP.period `S.is` S "from" +:+ refS frequencyDD