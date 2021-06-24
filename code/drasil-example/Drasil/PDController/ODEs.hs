module Drasil.PDController.ODEs where

import Language.Drasil.Code
import Language.Drasil.CodeExpr

import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Unitals


pidODEOptions :: ODEOptions
pidODEOptions
  = odeOptions RK45 (sy odeAbsTolConst) (sy odeRelTolConst) (sy qdStepTime) (exactDbl 0)

-- This is a second order ODE. The equation should be in the form of
-- variable substitution, i.e. u = y'. However here the the equation
-- can be defined in terms of the dependent variable itself because of the 
-- way scipy expects the function in python. 
pidODEInfo :: ODEInfo
pidODEInfo
  = odeInfo (quantvar time) (quantvar opProcessVariable)
      [quantvar ipPropGain, quantvar ipDerivGain, quantvar ipSetPt]
      (exactDbl 0)
      (sy qdSimTime)
      (exactDbl 0)
      [idx (sy opProcessVariable) (int 1),
      neg ((exactDbl 1 `addRe` sy qdDerivGain) `mulRe` idx (sy opProcessVariable) (int 1))   -- ? CHECK: Seems like `neg` does not generate generate sufficient parentheses?
      $- ((exactDbl 20 `addRe` sy qdPropGain) `mulRe` idx (sy opProcessVariable) (int 0))
      `addRe` (sy qdSetPointTD `mulRe` sy qdPropGain)]
      pidODEOptions
