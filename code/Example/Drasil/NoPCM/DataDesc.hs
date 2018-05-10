module Drasil.NoPCM.DataDesc (inputMod) where

import Language.Drasil
import Drasil.SWHS.Unitals (tank_length, diam, coil_SA, temp_C, w_density,
  htCap_W, coil_HTC, temp_init, tau, time_final, abs_tol, rel_tol, cons_tol)

inputMod :: Mod
inputMod = Mod "InputFormat" [nopcmInputData]

nopcmInputData :: Func
nopcmInputData = funcData "get_inputs" $
  [ junkLine,
    singleton [] tank_length,
    junkLine,
    singleton [] diam,
    junkLine,
    singleton [] coil_SA,
    junkLine,
    singleton [] temp_C,
    junkLine,
    singleton [] w_density,
    junkLine,
    singleton [] htCap_W,
    junkLine,
    singleton [] coil_HTC,
    junkLine,
    singleton [] temp_init,
    junkLine,
    singleton [] tau,
    junkLine,
    singleton [] time_final,
    junkLine,
    singleton [] abs_tol,
    junkLine,
    singleton [] rel_tol,
    junkLine,
    singleton [] cons_tol
  ]