module Drasil.GlassBR.Changes where

import Language.Drasil
import Drasil.GlassBR.Modules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8 :: LCChunk

lc1 = LCChunk (
  nw $ nc "hardware" "The specific hardware on which the software is running."
  ) [mod_hw]

lc2 = LCChunk ( nw $ nc "input" "The format of the initial input data." 
  ) [mod_inputf]

lc3 = LCChunk ( nw $ nc "parameters" "The format of the input parameters."
  ) [mod_inputp]

lc4 = LCChunk ( nw $ nc "output" "The format of the final output data."
  ) [mod_outputf]

lc5 = LCChunk ( 
  nw $ nc "equations" "How the equations are defined using the input parameters."
  ) [mod_calc]

lc6 = LCChunk (nw $ nc "control"
  "How the overall control of the calculations is orchestrated."
  ) [mod_ctrl]

lc7 = LCChunk ( nw $ nc "interpd" "The format of the data used for interpolation."
  ) [mod_interpd]

lc8 = LCChunk ( nw $ nc "interp" "The algorithm used for interpolation."
  ) [mod_interp]


uc1, uc2, uc3, uc4, uc5 :: UCChunk

uc1 = nw $ nc "IO" (
  "Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen)." )

uc2 = nw $ nc "inputsource" (
  "There will always be a source of input data external to the software." )

uc3 = nw $ nc "output" (
  "Output data are displayed to the output device." )

uc4 = nw $ nc "goal" (
  "The goal of the system is to predict whether the glass slab under " ++
  "consideration can withstand an explosion of a certain degree.")

uc5 = nw $ nc "equations" (
  "The equations for Safety can be defined using parameters defined " ++
  "in the input parameters module.")

