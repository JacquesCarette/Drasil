module Drasil.GlassBR.Changes where

import Language.Drasil
import Drasil.GlassBR.Modules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8 :: LCChunk

lc1 = LCChunk (nw $ npnc "hardware" (nounPhraseSP
  "The specific hardware on which the software is running."
  )) [mod_hw]

lc2 = LCChunk ( nw $ npnc "input" (nounPhraseSP
  "The format of the initial input data.")
  ) [mod_inputf]

lc3 = LCChunk ( nw $ npnc "parameters" (nounPhraseSP 
  "The format of the input parameters.")
  ) [mod_inputp]

lc4 = LCChunk ( nw $ npnc "output" (nounPhraseSP
  "The format of the final output data.")
  ) [mod_outputf]

lc5 = LCChunk ( 
  nw $ npnc "equations" (nounPhraseSP 
  "How the equations are defined using the input parameters.")
  ) [mod_calc]

lc6 = LCChunk (nw $ npnc "control" (nounPhraseSP
  "How the overall control of the calculations is orchestrated.")
  ) [mod_ctrl]

lc7 = LCChunk ( nw $ npnc "interpd" (nounPhraseSP
  "The format of the data used for interpolation.")
  ) [mod_interpd]

lc8 = LCChunk ( nw $ npnc "interp" (nounPhraseSP
  "The algorithm used for interpolation.")
  ) [mod_interp]


uc1, uc2, uc3, uc4, uc5 :: UCChunk

uc1 = nw $ npnc "IO" (nounPhraseSP $
  "Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen)." )

uc2 = nw $ npnc "inputsource" (nounPhraseSP
  "There will always be a source of input data external to the software." )

uc3 = nw $ npnc "output" (nounPhraseSP
  "Output data are displayed to the output device." )

uc4 = nw $ npnc "goal" (nounPhraseSP $
  "The goal of the system is to predict whether the glass slab under " ++
  "consideration can withstand an explosion of a certain degree.")

uc5 = nw $ npnc "equations" (nounPhraseSP $
  "The equations for Safety can be defined using parameters defined " ++
  "in the input parameters module.")

