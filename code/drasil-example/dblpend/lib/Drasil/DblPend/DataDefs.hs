{-# LANGUAGE PostfixOperators #-}

module Drasil.DblPend.DataDefs where

import Prelude hiding (sin, cos, sqrt)

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S
import Theory.Drasil (DataDefinition, ddMENoRefs, ddENoRefs)

import Drasil.DblPend.LabelledContent (figMotion)
import Drasil.DblPend.Unitals
  ( pendDisAngle_1, pendDisAngle_2
  , lenRod_1, lenRod_2
  , posVec_1, posVec_2
  , mvVel_1, mvVel_2
  , mvAccel_1, mvAccel_2
  , mvForce_1, mvForce_2
  , massObj_1, massObj_2
  )

import Drasil.DblPend.Concepts (pendulumPos)
import Drasil.DblPend.Expressions
  ( mvVelExpr_1, mvVelExpr_2
  , mvAccelExpr_1, mvAccelExpr_2
  )

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
-- Position Vectors
--------------------------------------------
positionVecDD_1 :: DataDefinition
positionVecDD_1 = ddENoRefs positionVecQD_1 Nothing "positionVecDD1" [posVecRef_1]

positionVecQD_1 :: SimpleQDef
positionVecQD_1 = mkQuantDef posVec_1 positionVecEqn_1

-- r₁ = l₁ * (sin θ₁ e₁ - cos θ₁ e₂)
positionVecEqn_1 :: PExpr
positionVecEqn_1 = sy lenRod_1 $* vec2D (sin (sy pendDisAngle_1)) (neg (cos (sy pendDisAngle_1)))

posVecRef_1 :: Sentence
posVecRef_1 = ch posVec_1 `S.isThe` phrase pendulumPos +:+. refS figMotion

--------------------------------------------
positionVecDD_2 :: DataDefinition
positionVecDD_2 = ddENoRefs positionVecQD_2 Nothing "positionVecDD2" [posVecRef_2]

positionVecQD_2 :: SimpleQDef
positionVecQD_2 = mkQuantDef posVec_2 positionVecEqn_2

-- r₂ = r₁ + l₂ * (sin θ₂ e₁ - cos θ₂ e₂)
positionVecEqn_2 :: PExpr
positionVecEqn_2 = sy posVec_1 $+ (sy lenRod_2 $* vec2D (sin (sy pendDisAngle_2)) (neg (cos (sy pendDisAngle_2))))

posVecRef_2 :: Sentence
posVecRef_2 = ch posVec_2 `S.isThe` phrase pendulumPos +:+. refS figMotion

--------------------------------------------
-- Velocity Vectors
--------------------------------------------
-- Velocity 1 depends on angularVel_1
velocityVecDD_1 :: DataDefinition
velocityVecDD_1 = ddMENoRefs velocityVecQD_1 Nothing "velocityVecDD1" []

velocityVecQD_1 :: ModelQDef
velocityVecQD_1 = mkQuantDef mvVel_1 mvVelExpr_1

-- Velocity 2 depends on angularVel_2 and velocity 1
velocityVecDD_2 :: DataDefinition
velocityVecDD_2 = ddMENoRefs velocityVecQD_2 Nothing "velocityVecDD2" []

velocityVecQD_2 :: ModelQDef
velocityVecQD_2 = mkQuantDef mvVel_2 mvVelExpr_2

--------------------------------------------
-- Acceleration Vectors
--------------------------------------------
-- Acceleration 1 depends on angularAccel_1
accelVecDD_1 :: DataDefinition
accelVecDD_1 = ddMENoRefs accelVecQD_1 Nothing "accelVecDD1" []

accelVecQD_1 :: ModelQDef
accelVecQD_1 = mkQuantDef mvAccel_1 mvAccelExpr_1

-- Acceleration 2 depends on angularAccel_2 and acceleration 1
accelVecDD_2 :: DataDefinition
accelVecDD_2 = ddMENoRefs accelVecQD_2 Nothing "accelVecDD2" []

accelVecQD_2 :: ModelQDef
accelVecQD_2 = mkQuantDef mvAccel_2 mvAccelExpr_2

--------------------------------------------
-- Force Vectors (F = m * a)
--------------------------------------------
-- Force 1 depends on acceleration 1
forceVecDD_1 :: DataDefinition
forceVecDD_1 = ddMENoRefs forceVecQD_1 Nothing "forceVecDD1" []

forceVecQD_1 :: ModelQDef
forceVecQD_1 = mkQuantDef mvForce_1 (sy massObj_1 $* sy mvAccel_1)

-- Force 2 depends on acceleration 2
forceVecDD_2 :: DataDefinition
forceVecDD_2 = ddMENoRefs forceVecQD_2 Nothing "forceVecDD2" []

forceVecQD_2 :: ModelQDef
forceVecQD_2 = mkQuantDef mvForce_2 (sy massObj_2 $* sy mvAccel_2)