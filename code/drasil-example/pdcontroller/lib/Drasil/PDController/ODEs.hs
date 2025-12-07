module Drasil.PDController.ODEs where

import Language.Drasil (LiteralC(exactDbl), ExprC(sy), InitialValueProblem, makeAIVP)
import Language.Drasil.Code (odeInfo', odeOptions, quantvar, ODEInfo,
    ODEMethod(RK45), ODEOptions)

import Drasil.PDController.Unitals (dqdSetPointTD, dqdPropGain, dqdDerivGain,
    dqdSimTime, dqdStepTime, odeRelTolConst, odeAbsTolConst)
import Drasil.PDController.IModel(imPDRC)

pidODEOptions :: ODEOptions
pidODEOptions = odeOptions
  RK45 (sy odeAbsTolConst) (sy odeRelTolConst) (sy dqdStepTime)

pdIVP :: InitialValueProblem
pdIVP = makeAIVP (exactDbl 0) (sy dqdSimTime) [exactDbl 0, exactDbl 0]

pidODEInfo :: ODEInfo
pidODEInfo = odeInfo'
  [quantvar dqdPropGain, quantvar dqdDerivGain, quantvar dqdSetPointTD]
  pidODEOptions
  imPDRC
  pdIVP
