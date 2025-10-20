{-# LANGUAGE PostfixOperators #-}

module Drasil.DblPend.Derivations where

import Prelude hiding (sin, cos)

import Language.Drasil (ModelExprC(..), ExprC(..), ModelExpr)
import Data.Drasil.Quantities.Physics (gravitationalMagnitude, time)
import Drasil.DblPend.Unitals
  ( lenRod_1, lenRod_2, pendDisAngle_1, pendDisAngle_2
  , massObj_1, massObj_2, tension_1, tension_2
  , posVec_1, posVec_2, mvVel_1, mvVel_2, mvAccel_1, mvAccel_2 )
import Drasil.DblPend.Expressions (vector)

-- =====================================================
-- Vector-based Derivations using Clifford Algebra
-- =====================================================

-- | Position vectors
posVecDerivEqn_1, posVecDerivEqn_2 :: ModelExpr
posVecDerivEqn_1 = sy posVec_1 $=
  vector (sy lenRod_1 `cScale` sin (sy pendDisAngle_1))
         (neg (sy lenRod_1 `cScale` cos (sy pendDisAngle_1)))

posVecDerivEqn_2 = sy posVec_2 $=
  sy posVec_1 $+
  vector (sy lenRod_2 `cScale` sin (sy pendDisAngle_2))
         (neg (sy lenRod_2 `cScale` cos (sy pendDisAngle_2)))


-- | Velocity vectors (first derivatives of position)
velVecDerivEqn_1, velVecDerivEqn_2 :: ModelExpr
velVecDerivEqn_1 = sy mvVel_1 $= deriv (sy posVec_1) time
velVecDerivEqn_2 = sy mvVel_2 $= deriv (sy posVec_2) time


-- | Acceleration vectors (derivatives of velocity)
accelVecDerivEqn_1, accelVecDerivEqn_2 :: ModelExpr
accelVecDerivEqn_1 = sy mvAccel_1 $= deriv (sy mvVel_1) time
accelVecDerivEqn_2 = sy mvAccel_2 $= deriv (sy mvVel_2) time


-- =====================================================
-- Angular acceleration derivations using vector forces
-- =====================================================

angularAccelDerivEqns :: [ModelExpr]
angularAccelDerivEqns =
  [ angAccelEqn_1
  , angAccelEqn_2
  , angAccelEqn_3
  , angAccelEqn_4
  ]

-- Simplified symbolic GA-based relationships
angAccelEqn_1, angAccelEqn_2, angAccelEqn_3, angAccelEqn_4 :: ModelExpr
angAccelEqn_1 = sy massObj_1 `cScale` sy mvAccel_1 $=
  neg (sy tension_1) `cScale` sin (sy pendDisAngle_1)

angAccelEqn_2 = sy massObj_1 `cScale` sy mvAccel_1 $=
  sy tension_1 `cScale` cos (sy pendDisAngle_1)
  $- (sy massObj_1 `cScale` sy gravitationalMagnitude)

angAccelEqn_3 = sy massObj_2 `cScale` sy mvAccel_2 $=
  neg (sy tension_2) `cScale` sin (sy pendDisAngle_2)

angAccelEqn_4 = sy massObj_2 `cScale` sy mvAccel_2 $=
  sy tension_2 `cScale` cos (sy pendDisAngle_2)
  $- (sy massObj_2 `cScale` sy gravitationalMagnitude)