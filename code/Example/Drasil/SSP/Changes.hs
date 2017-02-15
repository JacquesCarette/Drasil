module Drasil.SSP.Changes where

import Language.Drasil
import Drasil.SSP.Modules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13, lc14]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13
  , lc14 :: LCChunk

lc1 = LCChunk ( ncWDS "hardware"
    ( S "The specific hardware on which the software is running." )
  ) [mod_hw]

lc2 = LCChunk ( ncWDS "control"
    ( S "The algorithm for the overall operation procedure of the program." )
  ) [mod_ctrl]

lc3 = LCChunk ( ncWDS "input"
    ( S "The format of the initial input data." )
  ) [mod_inputf]

lc4 = LCChunk ( ncWDS "output"
    ( S "The format of the final output data." )
  ) [mod_outputf]

lc5 = LCChunk ( ncWDS "genalg"
    ( S "Algorithm for determining the critical slip surface." )
  ) [mod_genalg]

lc6 = LCChunk ( ncWDS "kin"
    ( S "Criteria for a slip surface to be considered physically " :+:
      S "realistic/kinematically admissible." )
  ) [mod_kinadm]

lc7 = LCChunk ( ncWDS "slicer"
    ( S "The algorithm for creating the slice points for slip surface analysis." )
  ) [mod_slipslicer]

lc8 = LCChunk ( ncWDS "fsweight"
    ( S "The weighting scheme for comparing slip surfaces factors of safety." )
  ) [mod_slipweight]

lc9 = LCChunk ( ncWDS "mp"
    ( S "The algorithm for implementation of the Morgenstern Price Solver." )
  ) [mod_mp]

lc10 = LCChunk ( ncWDS "rfem"
    ( S "The algorithm for implementation of the RFEM Solver." )
  ) [mod_rfem]

lc11 = LCChunk ( ncWDS "prop"
    ( S "The algorithm for assigning soil properties to slices." )
  ) [mod_sps]

lc12 = LCChunk ( ncWDS "array"
    ( S "The implementation for the sequence (array) data structure." )
  ) [mod_sds]

lc13 = LCChunk ( ncWDS "rand"
    ( S "The method of generating pseudo-random numbers." )
  ) [mod_rng]

lc14 = LCChunk ( ncWDS "plot"
    ( S "The method of displaying the final output." )
  ) [mod_plot]


uc1, uc2, uc3 :: UCChunk

uc1 = ncWDS "IO"
  ( S "Input/Output devices (Input: File and/or Keyboard, Output: File, " :+:
    S "Memory, and/or Screen)." )

uc2 = ncWDS "inputsource"
  ( S "There will always be a source of input data external to the software." )

uc3 = ncWDS "output"
  ( S "Output data are displayed to the output device." )
