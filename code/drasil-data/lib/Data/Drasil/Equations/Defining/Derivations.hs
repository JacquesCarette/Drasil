module Data.Drasil.Equations.Defining.Derivations where

import Language.Drasil (ExprC(..), LiteralC(..), ModelExpr, vec2D)
import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, gravitationalAccel, weight)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)

-- * Weight equation derivation
weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn, weightDerivSpecWeightEqn :: ModelExpr
weightDerivNewtonEqn      = sy QP.weight $= mul (sy QPP.mass) (sy QP.gravitationalAccel)
weightDerivReplaceMassEqn = sy QP.weight $= mul (sy QPP.density) (sy QPP.vol `mul` sy QP.gravitationalAccel)
weightDerivSpecWeightEqn  = sy QP.weight $= mul (sy QPP.vol) (sy QPP.specWeight)
weightDerivAccelEqn = sy QP.acceleration $= vec2D (exactDbl 0) (sy QP.gravitationalAccel `mul` sy QM.unitVectj)
