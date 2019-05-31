module Drasil.SWHS.DataDesc (inputMod) where
  

import Language.Drasil.Code (Func, Mod(Mod), funcData, junkLine, singleton)

import Drasil.SWHS.Unitals (tankLength, diam, pcmVol, pcmSA, pcmDensity,
    tempMeltP, htCapSP, htCapLP, htFusion, coilSA, tempC,
    wDensity, htCapW, coilHTC, pcmHTC, tempInit, tau, timeFinal,
    absTol, relTol, consTol)

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
    singleton  pcmDensity,
    junkLine, -- 11
    singleton  tempMeltP,
    junkLine, -- 13
    singleton  htCapSP,
    junkLine, -- 15
    singleton  htCapLP,
    junkLine, -- 17
    singleton  htFusion,
    junkLine, -- 19
    singleton  coilSA,
    junkLine, -- 21
    singleton  tempC,
    junkLine, -- 23
    singleton  wDensity,
    junkLine, -- 25
    singleton  htCapW,
    junkLine, -- 27
    singleton  coilHTC,
    junkLine, -- 29
    singleton  pcmHTC,
    junkLine, -- 31
    singleton  tempInit,
    junkLine, -- 33
    singleton  tau, -- FIXME: Not sure if tau should be used for "time-step"
    junkLine,-- 35
    singleton  timeFinal,
    junkLine, -- 37
    singleton  absTol,
    junkLine, -- 39
    singleton relTol,
    junkLine, -- 41
    singleton consTol
  ]

-- Numbering refers to corresopnding line on input file