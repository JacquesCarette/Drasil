module Drasil.DblPend.DataDefs where

import Control.Lens ((^.))

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (DataDefinition, ddENoRefs, ddMENoRefs)
import Drasil.DblPend.Figures (figMotion)
import Drasil.DblPend.Unitals (pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2, xPos_1, yPos_1, xPos_2, yPos_2, mvForce_1, mvAccel_1, massObj_1)
import Drasil.DblPend.Concepts (horizontalPos, verticalPos)
import Data.Drasil.Quantities.Physics (velocity, position, time, acceleration, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)

dataDefs :: [DataDefinition]
dataDefs = [velocityGDD, positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, accelGDD, forceGDD] -- Full list including Clifford algebra content

------------------------
-- Velocity in General--
------------------------
velocityGDD :: DataDefinition
velocityGDD = ddMENoRefs velocityGQD Nothing "velocityGDD" []

velocityGQD :: ModelQDef
velocityGQD = mkQuantDef velocity velocityGEqn

velocityGEqn :: ModelExpr
velocityGEqn = deriv (sy position) time

-------------------------------------------------
-- Position in X Direction in the First Object --
-------------------------------------------------
positionXDD_1 :: DataDefinition
positionXDD_1 = ddENoRefs positionXQD_1 Nothing "positionXDD1" [positionXRef_1, positionXFigRef_1]

positionXQD_1 :: SimpleQDef
positionXQD_1 = mkQuantDef xPos_1 positionXEqn_1

positionXEqn_1 :: PExpr
positionXEqn_1 = sy lenRod_1 $* sin (sy pendDisAngle_1)

positionXFigRef_1 :: Sentence
positionXFigRef_1 = ch xPos_1 `S.is` S "shown in" +:+. refS figMotion

positionXRef_1 :: Sentence
positionXRef_1 = ch xPos_1 `S.isThe` phrase horizontalPos

------------------------------------------------
-- Position in Y Direction in the First Object --
------------------------------------------------
positionYDD_1 :: DataDefinition
positionYDD_1 = ddENoRefs positionYQD_1 Nothing "positionYDD1" [positionYRef_1, positionYFigRef_1]

positionYQD_1 :: SimpleQDef
positionYQD_1 = mkQuantDef yPos_1 positionYEqn_1

positionYEqn_1 :: PExpr
positionYEqn_1 = neg (sy lenRod_1 $* cos (sy pendDisAngle_1))

positionYFigRef_1 :: Sentence
positionYFigRef_1 = ch yPos_1 `S.is` S "shown in" +:+. refS figMotion

positionYRef_1 :: Sentence
positionYRef_1 = ch yPos_1 `S.isThe` phrase verticalPos

-----------------------------------------------
-- Position in X Direction in the Second Object--
-----------------------------------------------
positionXDD_2 :: DataDefinition
positionXDD_2 = ddENoRefs positionXQD_2 Nothing "positionXDD2" [positionXRef_2, positionXFigRef_2]

positionXQD_2 :: SimpleQDef
positionXQD_2 = mkQuantDef xPos_2 positionXEqn_2

positionXEqn_2 :: PExpr
positionXEqn_2 = sy (positionXDD_1 ^. defLhs) $+ (sy lenRod_2 $* sin (sy pendDisAngle_2))

positionXFigRef_2 :: Sentence
positionXFigRef_2 = ch xPos_2 `S.is` S "shown in" +:+. refS figMotion

positionXRef_2 :: Sentence
positionXRef_2 = ch xPos_2 `S.isThe` phrase horizontalPos

-----------------------------------------------
-- Position in Y Direction in the Second Object--
-----------------------------------------------
positionYDD_2 :: DataDefinition
positionYDD_2 = ddENoRefs positionYQD_2 Nothing "positionYDD2" [positionYRef_2, positionYFigRef_2]

positionYQD_2 :: SimpleQDef
positionYQD_2 = mkQuantDef yPos_2 positionYEqn_2

positionYEqn_2 :: PExpr
positionYEqn_2 = sy (positionYDD_1 ^. defLhs) $+ neg (sy lenRod_2 $* cos (sy pendDisAngle_2))

positionYFigRef_2 :: Sentence
positionYFigRef_2 = ch yPos_2 `S.is` S "shown in" +:+. refS figMotion

positionYRef_2 :: Sentence
positionYRef_2 = ch yPos_2 `S.isThe` phrase verticalPos

---------------------------
-- Acceleration in General--
---------------------------
accelGDD :: DataDefinition
accelGDD = ddMENoRefs accelGQD Nothing "accelerationGDD" []

accelGQD :: ModelQDef
accelGQD = mkQuantDef acceleration accelGEqn

accelGEqn :: ModelExpr
accelGEqn = deriv (sy velocity) time 

---------------------------
-- Force in General--
---------------------------
forceGDD :: DataDefinition
forceGDD = ddENoRefs forceGQD Nothing "forceGDD" []

forceGQD :: SimpleQDef
forceGQD = mkQuantDef mvForce_1 forceGEqn  -- Use specific Clifford force quantity

forceGEqn :: PExpr
forceGEqn = sy mvForce_1  -- Simplified to avoid type issues for now
