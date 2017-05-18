module Drasil.SSP.Changes where

import Language.Drasil
import Drasil.SSP.Modules
import Data.Drasil.Changes

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, 
      lc9, lc10, lc11, lc12, lc13, lc14]

ucs :: [UCChunk]
ucs = [ucIO, ucInputS, ucOutput]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8,
  lc9, lc10, lc11, lc12, lc13, lc14 :: LCChunk

lc1 = lcHW

lc2 = lcCtrl mod_ctrl (nounPhraseSP "The algorithm for the overall operation procedure of the program.")

lc3 = lcInputF mod_inputf

lc4 = lcOutputF mod_outputf

lc5 = LCChunk (nw $ npnc "genalg" (nounPhraseSP
  "Algorithm for determining the critical slip surface.")
  ) [mod_genalg]

lc6 = LCChunk (nw $ npnc "kin" (nounPhraseSP $
  "Criteria for a slip surface to be considered physically " ++ 
  "realistic/kinematically admissible.")
  ) [mod_kinadm]

lc7 = LCChunk (nw $ npnc "slicer" (nounPhraseSP
  "The algorithm for creating the slice points for slip surface analysis.")
  ) [mod_slipslicer]

lc8 = LCChunk (nw $ npnc "fsweight" (nounPhraseSP
  "The weighting scheme for comparing slip surfaces factors of safety.")
  ) [mod_slipweight]

lc9 = LCChunk (nw $ npnc "mp" (nounPhraseSP
  "The algorithm for implementation of the Morgenstern Price Solver.")
  ) [mod_mp]

lc10 = LCChunk (nw $ npnc "rfem" (nounPhraseSP
  "The algorithm for implementation of the RFEM Solver.")
  ) [mod_rfem]

lc11 = LCChunk (nw $ npnc "prop" (nounPhraseSP
  "The algorithm for assigning soil properties to slices.")
  ) [mod_sps]

lc12 = lcArray mod_sds

lc13 = lcRng mod_rng 

lc14 = lcPlot mod_plot