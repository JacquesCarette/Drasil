module Drasil.GlassBR.Changes(likelyChanges, unlikelyChanges) where

import Language.Drasil

import Drasil.GlassBR.Modules (mod_inputf, mod_inputp, mod_outputf,
  mod_calc, mod_ctrl, mod_interpd, mod_interp)

import Data.Drasil.Changes (ucInputS, ucOutput, lcInterpd, lcInterp,
  ucIO, lcCtrl, lcEqns, lcOutputF, lcInputP, lcHW, lcInputF)

{--}

likelyChanges :: [LCChunk]
likelyChanges = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8]

unlikelyChanges :: [UCChunk]
unlikelyChanges = [uc1, uc2, uc3, uc4, uc5]

{-Likely Changes-}

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8 :: LCChunk

lc1 = lcHW

lc2 = lcInputF mod_inputf

lc3 = lcInputP mod_inputp

lc4 = lcOutputF mod_outputf

lc5 = lcEqns mod_calc

lc6 = lcCtrl mod_ctrl 
  (nounPhraseSP "How the overall control of the calculations is orchestrated.")

lc7 = lcInterpd mod_interpd

lc8 = lcInterp mod_interp

{-Unlikely Changes-}

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