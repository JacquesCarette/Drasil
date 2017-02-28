module Drasil.SWHS.Changes where

import Prelude hiding (id)
import Language.Drasil
import Drasil.SWHS.Modules
import Drasil.SWHS.Concepts
import Data.Drasil.Concepts.Math (ode)

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12 :: LCChunk

lc1 = LCChunk (nc "hardware" ("The specific hardware on which the " ++
  "software is running.")) [mod_hw]

lc2 = LCChunk (nc "inputformat" "The format of the initial input data.") 
  [mod_inputf]

lc3 = LCChunk (nc "parameters" "The format of the input parameters.") 
  [mod_inputp]

lc4 = LCChunk (nc "inputverification" "The constraints on the input parameters.")
  [mod_inputv]

lc5 = LCChunk (nc "outputformat" "The format of the final output data.")
  [mod_outputf]

lc6 = LCChunk (nc "outputverification" ("The constraints on the output results."))
  [mod_outputv]

lc7 = LCChunk (ncWDS "temp" (S "How the governing " :+: (getAcc ode) :+:
  S "s are defined using the input parameters.")) [mod_temp]

lc8 = LCChunk (nc "energy" ("How the energy equations are defined using " ++
  "the input parameters.")) [mod_ener]

lc9 = LCChunk (nc "control" ("How the overall control of the calculations" ++
  " is orchestrated.")) [mod_ctrl]

lc10 = LCChunk (nc "sequence" ("The implementation for the sequence " ++
  "(array) data structure.")) [mod_seq]

lc11 = LCChunk (ncWDS "ode" (S "The algorithm used for the " :+: 
  (getAcc ode) :+: S " solver.")) [mod_ode]

lc12 = LCChunk (nc "plot" "The implementation of plotting data.") [mod_plot]


uc1, uc2, uc3, uc4, uc5, uc6 :: UCChunk

uc1 = nc "IO" ("Input/Output devices (Input: File and/or Keyboard, Output" ++
  ": File, Memory, and/or Screen).")

uc2 = nc "inputsource" ("There will always be a source of input data " ++
  "external to the software.")

uc3 = nc "output" "Output data are displayed to the output device."

uc4 = nc "goal" ("The goal of the system is to calculate temperatures and" ++
  " energies.")

uc5 = ncWDS "odes" (S "The " :+: (getAcc ode) :+: S "s for " :+:
  S "temperature can be defined using parameters defined in the input " :+:
  S "parameters module.")

uc6 = nc "energy" ("The energy equations can be defined using the " ++
  "parameters defined in the input parameters module.")
