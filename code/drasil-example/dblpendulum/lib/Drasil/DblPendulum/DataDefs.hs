module Drasil.DblPendulum.DataDefs (dataDefs, positionGDD, positionIYDD_1, positionIXDD_1, positionIXDD_2, positionIYDD_2, 
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
dataDefs = [positionGDD, positionIXDD_1, positionIYDD_1, positionIXDD_2, positionIYDD_2,
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
positionIXDD_1 :: DataDefinition
positionIXDD_1 = ddNoRefs positionIXQD_1 Nothing "positionIX1DD" [positionIXRef_1, positionIXFigRef_1]

positionIXQD_1 :: QDefinition
positionIXQD_1 = mkQuantDef ixPos_1 positionIXEqn_1

positionIXEqn_1 :: Expr
positionIXEqn_1 = sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)

positionIXFigRef_1 :: Sentence
positionIXFigRef_1 = ch ixPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIXRef_1 :: Sentence
positionIXRef_1 = ch ixPos_1 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the First Object--
-----------------------------------------------
positionIYDD_1 :: DataDefinition
positionIYDD_1 = ddNoRefs positionIYQD_1 Nothing "positionIY1DD" [positionIYRef_1, positionIYFigRef_1]

positionIYQD_1 :: QDefinition
positionIYQD_1 = mkQuantDef iyPos_1 positionIYEqn_1

positionIYEqn_1 :: Expr
positionIYEqn_1 = neg (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))

positionIYFigRef_1 :: Sentence
positionIYFigRef_1 = ch iyPos_1 `S.is` S "shown in" +:+. refS figMotion

positionIYRef_1 :: Sentence
positionIYRef_1 = ch iyPos_1 `S.isThe` phrase verticalPos

-----------------------------------------------
-- Position in X Dirction in the Second Object--
-----------------------------------------------
positionIXDD_2 :: DataDefinition
positionIXDD_2 = ddNoRefs positionIXQD_2 Nothing "positionIX2DD" [positionIXRef_2, positionIXFigRef_2]

positionIXQD_2 :: QDefinition
positionIXQD_2 = mkQuantDef ixPos_2 positionIXEqn_2

positionIXEqn_2 :: Expr
positionIXEqn_2 = positionIXEqn_1 `addRe` (sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

positionIXFigRef_2 :: Sentence
positionIXFigRef_2 = ch ixPos_2 `S.is` S "shown in" +:+. refS figMotion

positionIXRef_2 :: Sentence
positionIXRef_2 = ch ixPos_2 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the Second Object--
-----------------------------------------------
positionIYDD_2 :: DataDefinition
positionIYDD_2 = ddNoRefs positionIYQD_2 Nothing "positionIY2DD" [positionIYRef_2, positionIYFigRef_2]

positionIYQD_2 :: QDefinition
positionIYQD_2 = mkQuantDef iyPos_2 positionIYEqn_2

positionIYEqn_2 :: Expr
positionIYEqn_2 = positionIYEqn_1 `addRe` neg (sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))

positionIYFigRef_2 :: Sentence
positionIYFigRef_2 = ch iyPos_2 `S.is` S "shown in" +:+. refS figMotion

positionIYRef_2 :: Sentence
positionIYRef_2 = ch iyPos_2 `S.isThe` phrase verticalPos

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