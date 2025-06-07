module Drasil.PDController.ODEs where

import Control.Lens ((^.))

import Language.Drasil.Code (odeInfo', odeOptions, quantvar, ODEInfo,
    ODEMethod(RK45), ODEOptions)
import Language.Drasil.CodeExpr (LiteralC(exactDbl), ExprC(sy))

import Drasil.PDController.Unitals (qdSetPointTD, qdPropGain, qdDerivGain,
    qdSimTime, qdStepTime, odeRelTolConst, odeAbsTolConst)
import Language.Drasil(InitialValueProblem, makeAIVP, defLhs)
import Drasil.PDController.IModel(imPDRC)


pidODEOptions :: ODEOptions
pidODEOptions = odeOptions 
  RK45 (sy $ odeAbsTolConst ^. defLhs) (sy $ odeRelTolConst ^. defLhs) (sy qdStepTime)

pdIVP :: InitialValueProblem
pdIVP = makeAIVP (exactDbl 0) (sy qdSimTime) [exactDbl 0, exactDbl 0]

pidODEInfo :: ODEInfo
pidODEInfo = odeInfo'
  [quantvar qdPropGain, quantvar qdDerivGain, quantvar qdSetPointTD]
  pidODEOptions
  imPDRC
  pdIVP
