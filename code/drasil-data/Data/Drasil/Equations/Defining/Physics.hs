module Data.Drasil.Equations.Defining.Physics where

import Language.Drasil

import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  force, gravitationalAccel, height, pressure, weight)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)

newtonSLRel, weightEqn, weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn,
  hsPressureEqn  :: Relation

newtonSLRel               = sy QP.force $= sy QPP.mass * sy QP.acceleration

weightEqn                 = sy QP.weight $= sy QPP.vol * sy QPP.specWeight
weightDerivNewtonEqn      = sy QP.weight $= sy QPP.mass * sy QP.gravitationalAccel
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density * sy QPP.vol * sy QP.gravitationalAccel
weightDerivSpecWeightEqn  = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

weightDerivAccelEqn       = sy QP.acceleration $= vec2D 0 (sy QP.gravitationalAccel * sy QM.unitVectj)

hsPressureEqn             = sy QP.pressure $= sy QPP.specWeight * sy QP.height

