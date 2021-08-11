module Drasil.DblPendulum.DataDefs (dataDefs, positionGDD, positionYDD_1, positionXDD_1, positionXDD_2, positionYDD_2, 
      accelGDD, forceGDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.SI_Units (second)
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.DblPendulum.Figures (figMotion)
import qualified Data.Drasil.Quantities.Physics as QP (ixPos, iyPos,
      frequency, period, angularFrequency)
import Drasil.DblPendulum.Unitals (pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2, xPos_1, yPos_1, xPos_2, yPos_2)
--import Data.Drasil.Concepts.Physics (pendulum)
import qualified Data.Drasil.Quantities.Math as QM (pi_)
import Drasil.DblPendulum.Concepts (horizontalPos, verticalPos)
import Data.Drasil.Quantities.Physics (velocity, position, time, acceleration, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)


dataDefs :: [DataDefinition]
dataDefs = [positionGDD, positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, 
  accelGDD, forceGDD]

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
positionXDD_1 :: DataDefinition
positionXDD_1 = ddNoRefs positionXQD_1 Nothing "positionXDD1" [positionXRef_1, positionXFigRef_1]

positionXQD_1 :: QDefinition
positionXQD_1 = mkQuantDef xPos_1 positionXEqn_1

positionXEqn_1 :: Expr
positionXEqn_1 = sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)

positionXFigRef_1 :: Sentence
positionXFigRef_1 = ch xPos_1 `S.is` S "shown in" +:+. refS figMotion

positionXRef_1 :: Sentence
positionXRef_1 = ch xPos_1 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the First Object--
-----------------------------------------------
positionYDD_1 :: DataDefinition
positionYDD_1 = ddNoRefs positionYQD_1 Nothing "positionYDD1" [positionYRef_1, positionYFigRef_1]

positionYQD_1 :: QDefinition
positionYQD_1 = mkQuantDef yPos_1 positionYEqn_1

positionYEqn_1 :: Expr
positionYEqn_1 = neg (sy lenRod_1 `mulRe` cos (sy pendDisAngle_1))

positionYFigRef_1 :: Sentence
positionYFigRef_1 = ch yPos_1 `S.is` S "shown in" +:+. refS figMotion

positionYRef_1 :: Sentence
positionYRef_1 = ch yPos_1 `S.isThe` phrase verticalPos

-----------------------------------------------
-- Position in X Dirction in the Second Object--
-----------------------------------------------
positionXDD_2 :: DataDefinition
positionXDD_2 = ddNoRefs positionXQD_2 Nothing "positionXDD2" [positionXRef_2, positionXFigRef_2]

positionXQD_2 :: QDefinition
positionXQD_2 = mkQuantDef xPos_2 positionXEqn_2

positionXEqn_2 :: Expr
positionXEqn_2 = sy positionXDD_1 `addRe` (sy lenRod_2 `mulRe` sin (sy pendDisAngle_2))

positionXFigRef_2 :: Sentence
positionXFigRef_2 = ch xPos_2 `S.is` S "shown in" +:+. refS figMotion

positionXRef_2 :: Sentence
positionXRef_2 = ch xPos_2 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Dirction in the Second Object--
-----------------------------------------------
positionYDD_2 :: DataDefinition
positionYDD_2 = ddNoRefs positionYQD_2 Nothing "positionYDD2" [positionYRef_2, positionYFigRef_2]

positionYQD_2 :: QDefinition
positionYQD_2 = mkQuantDef yPos_2 positionYEqn_2

positionYEqn_2 :: Expr
positionYEqn_2 = sy positionYDD_1 `addRe` neg (sy lenRod_2 `mulRe` cos (sy pendDisAngle_2))

positionYFigRef_2 :: Sentence
positionYFigRef_2 = ch yPos_2 `S.is` S "shown in" +:+. refS figMotion

positionYRef_2 :: Sentence
positionYRef_2 = ch yPos_2 `S.isThe` phrase verticalPos

---------------------------
-- Accleartion in General--
---------------------------
accelGDD :: DataDefinition
accelGDD = ddNoRefs accelGQD Nothing "accelerationGDD" []

accelGQD :: QDefinition
accelGQD = mkQuantDef acceleration accelGEqn

accelGEqn :: Expr
accelGEqn = deriv (sy velocity) time 

---------------------------
-- Force in General--
---------------------------
forceGDD :: DataDefinition
forceGDD = ddNoRefs forceGQD Nothing "forceGDD" []

forceGQD :: QDefinition
forceGQD = mkQuantDef force forceGEqn

forceGEqn :: Expr
forceGEqn = sy mass `mulRe` sy acceleration

-- ------------------------------------------------------
-- frequencyDD :: DataDefinition
-- frequencyDD = ddNoRefs frequencyDDQD Nothing "frequencyDD" [frequencyRef]

-- frequencyDDQD :: QDefinition
-- frequencyDDQD = mkQuantDef QP.frequency frequencyDDEqn

-- frequencyDDEqn :: Expr
-- frequencyDDEqn = recip_ $ sy QP.period

-- frequencyRef :: Sentence
-- frequencyRef = ch QP.frequency `S.isThe` S "number of back and forth swings in one" +:+ phrase second

-- ------------------------------------------------------

-- angFrequencyDD :: DataDefinition
-- angFrequencyDD = ddNoRefs angFrequencyDDQD Nothing "angFrequencyDD" [angFrequencyRef]

-- angFrequencyDDQD :: QDefinition
-- angFrequencyDDQD = mkQuantDef QP.angularFrequency angFrequencyDDEqn

-- angFrequencyDDEqn :: Expr
-- angFrequencyDDEqn = exactDbl 2 `mulRe` sy QM.pi_ $/ sy QP.period

-- angFrequencyRef :: Sentence
-- angFrequencyRef = ch QP.period `S.is` S "from" +:+ refS periodSHMDD

-- ------------------------------------------------------

-- periodSHMDD :: DataDefinition
-- periodSHMDD = ddNoRefs periodSHMDDQD Nothing "periodSHMDD" [periodSHMRef]

-- periodSHMDDQD :: QDefinition
-- periodSHMDDQD = mkQuantDef QP.period periodSHMDDEqn

-- periodSHMDDEqn :: Expr
-- periodSHMDDEqn = recip_ $ sy QP.frequency

-- periodSHMRef :: Sentence
-- periodSHMRef = ch QP.period `S.is` S "from" +:+ refS frequencyDD