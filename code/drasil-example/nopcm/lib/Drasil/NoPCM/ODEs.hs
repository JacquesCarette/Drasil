module Drasil.NoPCM.ODEs (noPCMODEOpts, noPCMODEInfo) where

import Language.Drasil (recip_, ExprC(mulRe, idx, sy, ($-)), LiteralC(int, exactDbl))
import Language.Drasil.Code (odeInfo, odeOptions, quantvar, ODEInfo,
  ODEMethod(RK45), ODEOptions)

import Data.Drasil.Quantities.Physics (time)

import Drasil.SWHS.Unitals (tauW, tempC, tempInit, tempW, timeFinal, timeStep,
  absTol, relTol)


noPCMODEOpts :: ODEOptions
noPCMODEOpts = odeOptions RK45 (sy absTol) (sy relTol) (sy timeStep)

noPCMODEInfo :: ODEInfo
noPCMODEInfo = odeInfo (quantvar time) (quantvar tempW)
  [quantvar tauW, quantvar tempC] (exactDbl 0) (sy timeFinal) [(sy tempInit)]
  [recip_ (sy tauW) `mulRe` (sy tempC $- idx (sy tempW) (int 0))] noPCMODEOpts
