module Drasil.PDController.ODEs where

import Language.Drasil (LiteralC(exactDbl), ExprC(sy), InitialValueProblem, makeAIVP)
import Language.Drasil.Code (odeInfo', odeOptions, quantvar, ODEInfo,
    ODEMethod(RK45), ODEOptions)

import Drasil.PDController.Unitals (qdSetPointTD, qdPropGain, qdDerivGain,
    qdSimTime, qdStepTime, dqdAbsTol, dqdRelTol)
import Drasil.PDController.IModel(imPDRC)


pidODEOptions :: ODEOptions
pidODEOptions = odeOptions 
  RK45 (sy dqdAbsTol) (sy dqdRelTol) (sy qdStepTime)

pdIVP :: InitialValueProblem
pdIVP = makeAIVP (exactDbl 0) (sy qdSimTime) [exactDbl 0, exactDbl 0]

pidODEInfo :: ODEInfo
pidODEInfo = odeInfo'
  [quantvar qdPropGain, quantvar qdDerivGain, quantvar qdSetPointTD]
  pidODEOptions
  imPDRC
  pdIVP
