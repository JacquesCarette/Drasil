module Drasil.DblPendulum.DataDefs (eDataDefs, meDataDefs,
  positionGDD, positionYDD_1, positionXDD_1, positionXDD_2, positionYDD_2,
  accelGDD, forceGDD) where

import Prelude hiding (sin, cos, sqrt)
import Language.Drasil
import qualified Utils.Drasil.Sentence as S
import Theory.Drasil (DataDefinition, ddNoRefs)
import Drasil.DblPendulum.Figures (figMotion)
import Drasil.DblPendulum.Unitals (pendDisAngle_1, pendDisAngle_2, lenRod_1, lenRod_2, xPos_1, yPos_1, xPos_2, yPos_2)
import Drasil.DblPendulum.Concepts (horizontalPos, verticalPos)
import Data.Drasil.Quantities.Physics (velocity, position, time, acceleration, force)
import Data.Drasil.Quantities.PhysicalProperties (mass)


eDataDefs :: [DataDefinition Expr]
eDataDefs = [positionXDD_1, positionYDD_1, positionXDD_2, positionYDD_2, forceGDD]

meDataDefs :: [DataDefinition ModelExpr]
meDataDefs = [positionGDD, accelGDD]

------------------------
-- Position in General--
------------------------
positionGDD :: DataDefinition ModelExpr
positionGDD = ddNoRefs positionGQD Nothing "positionGDD" []

positionGQD :: QDefinition ModelExpr
positionGQD = mkQuantDef velocity positionGEqn

positionGEqn :: ModelExpr
positionGEqn = deriv (sy position) time

-------------------------------------------------
-- Position in X Direction in the First Object --
-------------------------------------------------
positionXDD_1 :: DataDefinition Expr
positionXDD_1 = ddNoRefs positionXQD_1 Nothing "positionXDD1" [positionXRef_1, positionXFigRef_1]

positionXQD_1 :: QDefinition Expr
positionXQD_1 = mkQuantDef xPos_1 positionXEqn_1

positionXEqn_1 :: Expr
positionXEqn_1 = sy lenRod_1 `mulRe` sin (sy pendDisAngle_1)

positionXFigRef_1 :: Sentence
positionXFigRef_1 = ch xPos_1 `S.is` S "shown in" +:+. refS figMotion

positionXRef_1 :: Sentence
positionXRef_1 = ch xPos_1 `S.isThe` phrase horizontalPos

------------------------------------------------
-- Position in Y Dirction in the First Object --
------------------------------------------------
positionYDD_1 :: DataDefinition Expr
positionYDD_1 = ddNoRefs positionYQD_1 Nothing "positionYDD1" [positionYRef_1, positionYFigRef_1]

positionYQD_1 :: QDefinition Expr
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
positionXDD_2 :: DataDefinition Expr
positionXDD_2 = ddNoRefs positionXQD_2 Nothing "positionXDD2" [positionXRef_2, positionXFigRef_2]

positionXQD_2 :: QDefinition Expr
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
positionYDD_2 :: DataDefinition Expr
positionYDD_2 = ddNoRefs positionYQD_2 Nothing "positionYDD2" [positionYRef_2, positionYFigRef_2]

positionYQD_2 :: QDefinition Expr
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
accelGDD :: DataDefinition ModelExpr
accelGDD = ddNoRefs accelGQD Nothing "accelerationGDD" []

accelGQD :: QDefinition ModelExpr
accelGQD = mkQuantDef acceleration accelGEqn

accelGEqn :: ModelExpr
accelGEqn = deriv (sy velocity) time 

---------------------------
-- Force in General--
---------------------------
forceGDD :: DataDefinition Expr
forceGDD = ddNoRefs forceGQD Nothing "forceGDD" []

forceGQD :: QDefinition Expr
forceGQD = mkQuantDef force forceGEqn

forceGEqn :: Expr
forceGEqn = sy mass `mulRe` sy acceleration
