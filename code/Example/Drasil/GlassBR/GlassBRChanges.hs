module Example.Drasil.GlassBR.GlassBRChanges where

import Language.Drasil
import Example.Drasil.GlassBR.GlassBRModules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8 :: LCChunk

lc1 = LCChunk ( CC "hardware"
    ( S "The specific hardware on which the software is running." )
  ) [mod_hw]

lc2 = LCChunk ( CC "input"
    ( S "The format of the initial input data." )
  ) [mod_inputf]

lc3 = LCChunk ( CC "parameters"
    ( S "The format of the input parameters.")
  ) [mod_inputp]

lc4 = LCChunk ( CC "output"
    ( S "The format of the final output data." )
  ) [mod_outputf]

lc5 = LCChunk ( CC "equations"
    ( S "How the equations are defined using the input parameters.")
  ) [mod_calc]

lc6 = LCChunk (CC "control"
    (S "How the overall control of the calculations is orchestrated.")
  ) [mod_ctrl]

lc7 = LCChunk ( CC "interpd"
    ( S "The format of the data used for interpolation." )
  ) [mod_interpd]

lc8 = LCChunk ( CC "interp"
    ( S "The algorithm used for interpolation.")
  ) [mod_interp]


uc1, uc2, uc3, uc4, uc5 :: UCChunk

uc1 = CC "IO"
  ( S "Input/Output devices (Input: File and/or Keyboard, Output: File, " :+:
    S "Memory, and/or Screen)." )

uc2 = CC "inputsource"
  ( S "There will always be a source of input data external to the software." )

uc3 = CC "output"
  ( S "Output data are displayed to the output device." )

uc4 = CC "goal"
  ( S "The goal of the system is to predict whether the glass slab under " :+:
    S "consideration can withstand an explosion of a certain degree.")

uc5 = CC "equations"
  ( S "The equations for Safety can be defined using parameters defined " :+:
    S "in the input parameters module.")

