module Drasil.NoPCM.DataDesc where

import Language.Drasil
import Drasil.SWHS.Unitals

inputMod :: Mod
inputMod = ModData "InputFormat" [nopcmInputData]

nopcmInputData :: DataDesc
nopcmInputData =
  [ junkLine,
    singleton tank_length,
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
    singleton temp_init,
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