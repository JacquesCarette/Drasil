module Drasil.DblPendulum.DataDefs (dataDefs, positionIY_1, positionIX_1, angFrequencyDD,
         frequencyDD, periodSHMDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos,
      frequency, period, angularFrequency)
import Drasil.DblPendulum.Unitals (lenRod, initialPendAngle, lenRod_1, lenRod_2, ixPos_1, ixPos_2, iyPos_1, iyPos_2,
  initPendAngle_1, initPendAngle_2)
--import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Concepts (horizontalPos, verticalPos)


dataDefs :: [DataDefinition]
dataDefs = [positionIX_1, positionIY_1, frequencyDD, angFrequencyDD, periodSHMDD]

-----------------------------------------------
-- Position in X Dirction in the First Object--
-----------------------------------------------
positionIX_1 :: DataDefinition
positionIX_1 = ddNoRefs positionIXQD_1 Nothing "positionIX1" [positionIX1Ref, positionIXFigRef]

positionIXQD_1 :: QDefinition
positionIXQD_1 = mkQuantDef ixPos_1 positionIXEqn_1

positionIXEqn_1 :: Expr
positionIXEqn_1 = sy lenRod_1 `mulRe` sin (sy initPendAngle_1)

positionIXFigRef :: Sentence
positionIXFigRef = ch ixPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIX1Ref :: Sentence
positionIX1Ref = ch ixPos_1 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the First Object--
-----------------------------------------------
positionIY_1 :: DataDefinition
positionIY_1 = ddNoRefs positionIYQD_1 Nothing "positionIY1" [positionIY1Ref, positionIYFigRef]

positionIYQD_1 :: QDefinition
positionIYQD_1 = mkQuantDef iyPos_1 positionIYEqn_1

positionIYEqn_1 :: Expr
positionIYEqn_1 = neg (sy lenRod_1 `mulRe` cos (sy initPendAngle_1))

positionIYFigRef :: Sentence
positionIYFigRef = ch iyPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIY1Ref :: Sentence
positionIY1Ref = ch iyPos_1 `S.isThe` phrase verticalPos

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