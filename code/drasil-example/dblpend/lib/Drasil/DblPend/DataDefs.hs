{-# LANGUAGE PostfixOperators #-}

module Drasil.DblPend.DataDefs where

-- import Control.Lens ((^.))
import Prelude hiding (sin, cos, sqrt)

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (DataDefinition, ddENoRefs, ddMENoRefs)

import Drasil.DblPend.LabelledContent (figMotion)
import Drasil.DblPend.Unitals
  ( posVec_1, posVec_2
  , mvVel_1, mvVel_2
  , mvAccel_1, mvAccel_2
  , mvForce_1, mvForce_2
  )
import Drasil.DblPend.Expressions (mvPosExpr_1, mvPosExpr_2)

import Drasil.DblPend.Concepts (pendulumPos)
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)

------------------------------------------------------
-- Full List of Data Definitions (GA-based only)
------------------------------------------------------
dataDefs :: [DataDefinition]
dataDefs =
  [ positionVecDD_1
  , positionVecDD_2
  , velocityVecDD_1
  , velocityVecDD_2
  , accelVecDD_1
  , accelVecDD_2
  , forceVecDD_1
  , forceVecDD_2
  ]

--------------------------------------------
-- Position Vectors for Each Pendulum Bob --
--------------------------------------------

positionVecDD_1 :: DataDefinition
positionVecDD_1 =
  ddENoRefs positionVecQD_1 Nothing "positionVecDD1" [posVecRef_1]

positionVecQD_1 :: SimpleQDef
positionVecQD_1 = mkQuantDef posVec_1 mvPosExpr_1

posVecRef_1 :: Sentence
posVecRef_1 = ch posVec_1 `S.isThe` phrase pendulumPos +:+. refS figMotion

--------------------------------------------

positionVecDD_2 :: DataDefinition
positionVecDD_2 =
  ddENoRefs positionVecQD_2 Nothing "positionVecDD2" [posVecRef_2]

positionVecQD_2 :: SimpleQDef
positionVecQD_2 = mkQuantDef posVec_2 mvPosExpr_2

posVecRef_2 :: Sentence
posVecRef_2 = ch posVec_2 `S.isThe` phrase pendulumPos +:+. refS figMotion

--------------------------------------------
-- Velocity Vectors --
--------------------------------------------

velocityVecDD_1 :: DataDefinition
velocityVecDD_1 =
  ddMENoRefs velocityVecQD_1 Nothing "velocityVecDD1" []

velocityVecQD_1 :: ModelQDef
velocityVecQD_1 = mkQuantDef mvVel_1 velocityVecEqn_1

velocityVecEqn_1 :: ModelExpr
velocityVecEqn_1 = deriv (sy posVec_1) time

--------------------------------------------

velocityVecDD_2 :: DataDefinition
velocityVecDD_2 =
  ddMENoRefs velocityVecQD_2 Nothing "velocityVecDD2" []

velocityVecQD_2 :: ModelQDef
velocityVecQD_2 = mkQuantDef mvVel_2 velocityVecEqn_2

velocityVecEqn_2 :: ModelExpr
velocityVecEqn_2 = deriv (sy posVec_2) time

--------------------------------------------
-- Acceleration Vectors --
--------------------------------------------

accelVecDD_1 :: DataDefinition
accelVecDD_1 =
  ddMENoRefs accelVecQD_1 Nothing "accelVecDD1" []

accelVecQD_1 :: ModelQDef
accelVecQD_1 = mkQuantDef mvAccel_1 accelVecEqn_1

accelVecEqn_1 :: ModelExpr
accelVecEqn_1 = deriv (sy mvVel_1) time

--------------------------------------------

accelVecDD_2 :: DataDefinition
accelVecDD_2 =
  ddMENoRefs accelVecQD_2 Nothing "accelVecDD2" []

accelVecQD_2 :: ModelQDef
accelVecQD_2 = mkQuantDef mvAccel_2 accelVecEqn_2

accelVecEqn_2 :: ModelExpr
accelVecEqn_2 = deriv (sy mvVel_2) time

--------------------------------------------
-- Force Vectors --
--------------------------------------------

forceVecDD_1 :: DataDefinition
forceVecDD_1 =
  ddMENoRefs forceVecQD_1 Nothing "forceVecDD1" []

forceVecQD_1 :: ModelQDef
forceVecQD_1 = mkQuantDef mvForce_1 forceVecEqn_1

-- F₁ = m₁ * a₁
forceVecEqn_1 :: ModelExpr
forceVecEqn_1 = sy mass $* sy mvAccel_1

--------------------------------------------

forceVecDD_2 :: DataDefinition
forceVecDD_2 =
  ddMENoRefs forceVecQD_2 Nothing "forceVecDD2" []

forceVecQD_2 :: ModelQDef
forceVecQD_2 = mkQuantDef mvForce_2 forceVecEqn_2

-- F₂ = m₂ * a₂
forceVecEqn_2 :: ModelExpr
forceVecEqn_2 = sy mass $* sy mvAccel_2