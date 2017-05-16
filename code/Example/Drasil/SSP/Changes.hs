module Drasil.SSP.Changes where

import Language.Drasil
import Drasil.SSP.Modules
import Data.Drasil.Modules

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13, lc14]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13
  , lc14 :: LCChunk

lc1 = LCChunk ( nw $ npnc "hardware" (nounPhraseSP
  "The specific hardware on which the software is running." )
  ) [mod_hw]

lc2 = LCChunk ( nw $ npnc "control" (nounPhraseSP
  "The algorithm for the overall operation procedure of the program." )
  ) [mod_ctrl]

lc3 = LCChunk ( nw $ npnc "input" (nounPhraseSP
  "The format of the initial input data." )
  ) [mod_inputf]

lc4 = LCChunk ( nw $ npnc "output" (nounPhraseSP
  "The format of the final output data." )
  ) [mod_outputf]

lc5 = LCChunk ( nw $ npnc "genalg" (nounPhraseSP
  "Algorithm for determining the critical slip surface." )
  ) [mod_genalg]

lc6 = LCChunk ( nw $ npnc "kin" (nounPhraseSP $
  "Criteria for a slip surface to be considered physically " ++ 
  "realistic/kinematically admissible." )
  ) [mod_kinadm]

lc7 = LCChunk ( nw $ npnc "slicer" (nounPhraseSP
  "The algorithm for creating the slice points for slip surface analysis." )
  ) [mod_slipslicer]

lc8 = LCChunk ( nw $ npnc "fsweight" (nounPhraseSP
  "The weighting scheme for comparing slip surfaces factors of safety." )
  ) [mod_slipweight]

lc9 = LCChunk ( nw $ npnc "mp" (nounPhraseSP
  "The algorithm for implementation of the Morgenstern Price Solver." )
  ) [mod_mp]

lc10 = LCChunk ( nw $ npnc "rfem" (nounPhraseSP
  "The algorithm for implementation of the RFEM Solver." )
  ) [mod_rfem]

lc11 = LCChunk ( nw $ npnc "prop" (nounPhraseSP
  "The algorithm for assigning soil properties to slices." )
  ) [mod_sps]

lc12 = LCChunk ( nw $ npnc "array" (nounPhraseSP
  "The implementation for the sequence (array) data structure." )
  ) [mod_sds]

lc13 = LCChunk ( nw $ npnc "rand" (nounPhraseSP
  "The method of generating pseudo-random numbers." )
  ) [mod_rng]

lc14 = LCChunk ( nw $ npnc "plot" (nounPhraseSP
  "The method of displaying the final output." )
  ) [mod_plot]


uc1, uc2, uc3 :: UCChunk

uc1 = nw $ npnc "IO" (nounPhraseSP $
  "Input/Output devices (Input: File and/or Keyboard, Output: File, " ++
  "Memory, and/or Screen)." )

uc2 = nw $ npnc "inputsource" (nounPhraseSP
  "There will always be a source of input data external to the software." )

uc3 = nw $ npnc "output" (nounPhraseSP
  "Output data are displayed to the output device." )
