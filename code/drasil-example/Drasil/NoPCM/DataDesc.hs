module Drasil.NoPCM.DataDesc (inputMod) where

import Language.Drasil.Code (Func, Mod(Mod), funcData, junkLine, singleton)
import Drasil.SWHS.Unitals (tankLength, diam, coil_SA, temp_C, w_density,
  htCap_W, coil_HTC, tempInit, tau, time_final, abs_tol, rel_tol, cons_tol)

inputMod :: Mod
inputMod = Mod "InputFormat" [inputData]

inputData :: Func
inputData = funcData "get_inputs"
  [ junkLine,
    singleton tankLength,
    junkLine,
    singleton diam,
    junkLine,
    singleton coil_SA,
    junkLine,
    singleton temp_C,
    junkLine,
    singleton w_density,
    junkLine,
    singleton htCap_W,
    junkLine,
    singleton coil_HTC,
    junkLine,
    singleton tempInit,
    junkLine,
    singleton tau,
    junkLine,
    singleton time_final,
    junkLine,
    singleton abs_tol,
    junkLine,
    singleton rel_tol,
    junkLine,
    singleton cons_tol
  ]
