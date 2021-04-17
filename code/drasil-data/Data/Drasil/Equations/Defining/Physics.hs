module Data.Drasil.Equations.Defining.Physics where

-- We define both some basic equations of physics, and their wrappers as concepts
--
import Language.Drasil

import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, time,
  force, gravitationalAccel, height, pressure, weight, velocity, position)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)

------------------------------------------------------------------------------------------------------
-- The equations

newtonSLRel, weightEqn, weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn,
  weightDerivSpecWeightEqn,
  hsPressureEqn, accelerationEqn, velocityEqn  :: Relation

newtonSLRel               = sy QP.force $= sy QPP.mass * sy QP.acceleration

weightEqn                 = sy QP.weight $= sy QPP.vol * sy QPP.specWeight
weightDerivNewtonEqn      = sy QP.weight $= sy QPP.mass * sy QP.gravitationalAccel
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density * sy QPP.vol * sy QP.gravitationalAccel
weightDerivSpecWeightEqn  = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

weightDerivAccelEqn       = sy QP.acceleration $= vec2D 0 (sy QP.gravitationalAccel * sy QM.unitVectj)

hsPressureEqn             = sy QP.pressure $= sy QPP.specWeight * sy QP.height

velocityEqn               = sy QP.velocity $= deriv (sy QP.position) QP.time
accelerationEqn           = sy QP.acceleration $= deriv (sy QP.velocity) QP.time

------------------------------------------------------------------------------------------------------
-- The concepts

accelerationRC, velocityRC :: RelationConcept

accelerationRC = addRelToCC QP.acceleration "accelerationRC" accelerationEqn
velocityRC = addRelToCC QP.velocity "velocityRC" velocityEqn

