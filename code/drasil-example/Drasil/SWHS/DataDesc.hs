module Drasil.SWHS.DataDesc (inputMod) where

import Language.Drasil.Code (Func, Mod(Mod), funcData, junkLine, singleton)

import Drasil.SWHS.Unitals (tankLength, diam, pcmVol, pcmSA, pcm_density,
    temp_melt_P, htCap_S_P, htCap_L_P, htFusion, coil_SA, temp_C,
    w_density, htCap_W, coil_HTC, pcm_HTC, tempInit, tau, time_final,
    abs_tol, rel_tol, cons_tol)

inputMod :: Mod
inputMod = Mod "InputFormat" [inputData]

inputData :: Func
inputData = funcData "get_inputs"
  [ junkLine, -- 1
    singleton tankLength,
    junkLine, -- 3
    singleton  diam,
    junkLine, -- 5
    singleton  pcmVol,
    junkLine, -- 7
    singleton  pcmSA,
    junkLine, -- 9
    singleton  pcm_density,
    junkLine, -- 11
    singleton  temp_melt_P,
    junkLine, -- 13
    singleton  htCap_S_P,
    junkLine, -- 15
    singleton  htCap_L_P,
    junkLine, -- 17
    singleton  htFusion,
    junkLine, -- 19
    singleton  coil_SA,
    junkLine, -- 21
    singleton  temp_C,
    junkLine, -- 23
    singleton  w_density,
    junkLine, -- 25
    singleton  htCap_W,
    junkLine, -- 27
    singleton  coil_HTC,
    junkLine, -- 29
    singleton  pcm_HTC,
    junkLine, -- 31
    singleton  tempInit,
    junkLine, -- 33
    singleton  tau, -- FIXME: Not sure if tau should be used for "time-step"
    junkLine,-- 35
    singleton  time_final,
    junkLine, -- 37
    singleton  abs_tol,
    junkLine, -- 39
    singleton rel_tol,
    junkLine, -- 41
    singleton cons_tol
  ]

-- Numbering refers to corresopnding line on input file