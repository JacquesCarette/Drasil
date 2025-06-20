module Drasil.DblPend.DataDefs where

import Control.Lens ((^.))

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (DataDefinition, ddENoRefs, ddMENoRefs)
import Drasil.DblPend.Figures (figMotion)
import Drasil.DblPend.Unitals (pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2, xPos_1, yPos_1, xPos_2, yPos_2, angularVel_1, angularVel_2, xVel_1, yVel_1)
import Drasil.DblPend.Concepts (horizontalPos, verticalPos)
import Data.Drasil.Quantities.Physics (velocity, position, time, acceleration, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)

dataDefs :: [DataDefinition]
dataDefs = [positionGDD, positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, accelGDD, forceGDD]

------------------------
-- Position in General--
------------------------
positionGDD :: DataDefinition
positionGDD = ddMENoRefs positionGQD Nothing "positionGDD" []

positionGQD :: ModelQDef
positionGQD = mkQuantDef velocity positionGEqn

positionGEqn :: ModelExpr
positionGEqn = deriv (sy position) time

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
-- Position in Y Dirction in the First Object --
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
-- Position in X Dirction in the Second Object--
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
-- Position in Y Dirction in the Second Object--
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
-- Accleartion in General--
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
forceGQD = mkQuantDef force forceGEqn

forceGEqn :: PExpr
forceGEqn = vScale (sy mass) (sy acceleration)

-- NOTE:
-- Ideally, this definition would be written at the vector level:
--     velocity = d(position)/dt
-- However, since Drasil does not yet support expressing full vector derivatives
-- directly, we define velocity component-wise in terms of x and y.
-- This should be updated once vector-level definitions are supported.

velVecEqn_1 :: PExpr
velVecEqn_1 = vec2D
  (sy angularVel_1 $* sy lenRod_1 $* cos (sy pendDisAngle_1))
  (sy angularVel_1 $* positionXEqn_1)

velVecEqn_2 :: PExpr
velVecEqn_2 = vec2D
  (sy xVel_1 $+ (sy angularVel_2 $* sy lenRod_2 $* cos (sy pendDisAngle_2)))
  (sy yVel_1 $+ (sy angularVel_2 $* sy lenRod_2 $* sin (sy pendDisAngle_2)))



