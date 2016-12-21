module Drasil.SWHS.Changes where

import Control.Lens ((^.))
import Prelude hiding (id)
import Language.Drasil
import Drasil.SWHS.Modules
import Drasil.SWHS.Concepts

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12 :: LCChunk

lc1 = LCChunk (CC "hardware" (S "The specific hardware on which the " :+:
      S "software is running.")) [mod_hw]

lc2 = LCChunk (CC "inputformat" (S "The format of the initial input data.")) 
      [mod_inputf]

lc3 = LCChunk (CC "parameters" (S "The format of the input parameters.")) 
      [mod_inputp]

lc4 = LCChunk (CC "inputverification" (S "The constraints on the input " :+:
      S "parameters.")) [mod_inputv]

lc5 = LCChunk (CC "outputformat" (S "The format of the final output data."))
      [mod_outputf]

lc6 = LCChunk (CC "outputverification" (S "The constraints on the output " :+:
      S "results.")) [mod_outputv]

lc7 = LCChunk (CC "temp" (S "How the governing " :+: S (ordDiffEq ^. id) :+:
      S "s are defined using the input parameters.")) [mod_temp]

lc8 = LCChunk (CC "energy" (S "How the energy equations are defined using " :+:
      S "the input parameters.")) [mod_ener]

lc9 = LCChunk (CC "control" (S "How the overall control of the calculations" :+:
      S " is orchestrated.")) [mod_ctrl]

lc10 = LCChunk (CC "sequence" (S "The implementation for the sequence " :+:
       S "(array) data structure.")) [mod_seq]

lc11 = LCChunk (CC "ode" (S "The algorithm used for the " :+: 
       S (ordDiffEq ^. id) :+: S " solver.")) [mod_ode]

lc12 = LCChunk (CC "plot" (S "The implementation of plotting data.")) [mod_plot]


uc1, uc2, uc3, uc4, uc5, uc6 :: UCChunk

uc1 = CC "IO" (S "Input/Output devices (Input: File and/or Keyboard, Output" :+:
      S ": File, Memory, and/or Screen).")

uc2 = CC "inputsource" (S "There will always be a source of input data " :+:
      S "external to the software.")

uc3 = CC "output" (S "Output data are displayed to the output device.")

uc4 = CC "goal" (S "The goal of the system is to calculate temperatures and" :+:
      S " energies.")

uc5 = CC "odes" (S "The " :+: S (ordDiffEq ^. id) :+: S "s for " :+:
      S "temperature can be defined using parameters defined in the input " :+:
      S "parameters module.")

uc6 = CC "energy" (S "The energy equations can be defined using the " :+:
      S "parameters defined in the input parameters module.")