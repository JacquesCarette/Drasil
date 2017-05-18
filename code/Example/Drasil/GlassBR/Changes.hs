module Drasil.GlassBR.Changes where

import Language.Drasil

import Drasil.GlassBR.Modules
import Data.Drasil.Changes

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8 :: LCChunk

lc1 = lcHW

lc2 = lcInputF mod_inputf

lc3 = LCChunk ( nw $ npnc "parameters" (nounPhraseSP 
  "The format of the input parameters.")
  ) [mod_inputp]

lc4 = lcOutputF mod_outputf

lc5 = LCChunk ( 
  nw $ npnc "equations" (nounPhraseSP 
  "How the equations are defined using the input parameters.")
  ) [mod_calc]

lc6 = lcCtrl mod_ctrl (nounPhraseSP "How the overall control of the calculations is orchestrated.")

lc7 = LCChunk ( nw $ npnc "interpd" (nounPhraseSP
  "The format of the data used for interpolation.")
  ) [mod_interpd]

lc8 = LCChunk ( nw $ npnc "interp" (nounPhraseSP
  "The algorithm used for interpolation.")
  ) [mod_interp]


uc1, uc2, uc3, uc4, uc5 :: UCChunk

uc1 = ucIO

uc2 = ucInputS

uc3 = ucOutput

uc4 = nw $ npnc "goal" (nounPhraseSP $
  "The goal of the system is to predict whether the glass slab under " ++
  "consideration can withstand an explosion of a certain degree.")

uc5 = nw $ npnc "equations" (nounPhraseSP $
  "The equations for Safety can be defined using parameters defined " ++
  "in the input parameters module.")