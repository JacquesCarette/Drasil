module Drasil.SWHSNoPCM.ODEs (noPCMODEOpts, noPCMODEInfo) where

import Language.Drasil (ExprC(sy),LiteralC(exactDbl), InitialValueProblem, makeAIVP)
import Language.Drasil.Code (odeInfo', odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)
import Drasil.SWHS.Unitals (tauW, tempC, tempInit, timeFinal, timeStep, absTol, relTol)
import Drasil.SWHSNoPCM.IMods(eBalanceOnWtrRC)


noPCMODEOpts :: ODEOptions
noPCMODEOpts = odeOptions
  RK45 (sy absTol) (sy relTol) (sy timeStep)

noPCMIVP :: InitialValueProblem
noPCMIVP = makeAIVP (exactDbl 0) (sy timeFinal) [sy tempInit]

noPCMODEInfo :: ODEInfo
noPCMODEInfo = odeInfo'
  [quantvar tauW, quantvar tempC]
  noPCMODEOpts
  eBalanceOnWtrRC
  noPCMIVP
