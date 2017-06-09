module Drasil.SWHS.Changes where

import Prelude hiding (id)
import Language.Drasil
import Drasil.SWHS.Modules
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Changes

lcs :: [LCChunk]
lcs = [lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12]

ucs :: [UCChunk]
ucs = [uc1, uc2, uc3, uc4, uc5, uc6]

lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12 :: LCChunk

lc1 = lcHW

--lc2 = LCChunk (nw $ npnc "inputformat" (nounPhraseSP
--  "The format of the initial input data."))
--  [mod_inputf]
lc2 = lcInputF  mod_inputf

lc3 = LCChunk (nw $ npnc "parameters" (nounPhraseSP 
  "The format of the input parameters."))
  [mod_inputp]

lc4 = LCChunk (nw $ npnc "inputverification" (nounPhraseSP
  "The constraints on the input parameters."))
  [mod_inputv]

--lc5 = LCChunk (nw $ npnc "outputformat" (nounPhraseSP
--  "The format of the final output data."))
--  [mod_outputf]

lc5 = lcOutputF mod_outputf

lc6 = LCChunk (nw $ npnc "outputverification" (nounPhraseSP
  "The constraints on the output results."))
  [mod_outputv]

lc7 = LCChunk (nw $ npnc "temp" lc7np) [mod_temp]

lc8 = LCChunk (nw $ npnc "energy" (nounPhraseSP
  "How the energy equations are defined using the input parameters.")) [mod_ener]

lc9 = LCChunk (nw $ npnc "control" (nounPhraseSP
  "How the overall control of the calculations is orchestrated.")) [mod_ctrl]

--lc10 = LCChunk (nw $ npnc "sequence" (nounPhraseSP
--  "The implementation for the sequence (array) data structure.")) [mod_seq]
lc10 = lcArray mod_seq

lc11 = LCChunk (nw $ npnc "ode" lc11np) [mod_ode]

lc12 = LCChunk (nw $ npnc "plot" (nounPhraseSP 
  "The implementation of plotting data.")) [mod_plot]

--FIXME: These shouldn't be duplicated.
lc7np, lc11np, uc5np :: NP
lc7np = nounPhrase'' (S "How the governing" +:+ getAcc ode :+: 
  S "s are defined using the input parameters.") (S "How the governing" +:+ 
  (getAcc ode) :+: S "s are defined using the input parameters.") 
  CapFirst CapWords
  
lc11np = nounPhrase'' (S "The algorithm used for the" +:+ getAcc ode +:+
  S "solver.") (S "The algorithm used for the" +:+ getAcc ode +:+
  S "solver.") CapFirst CapWords
  
uc5np = nounPhrase'' (S "The" +:+ getAcc ode :+: S "s for" +:+
  S "temperature can be defined using parameters defined in the input" +:+
  S "parameters module.") (S "The" +:+ getAcc ode :+: S "s for" +:+
  S "temperature can be defined using parameters defined in the input" +:+
  S "parameters module.") CapFirst CapWords
  
uc1, uc2, uc3, uc4, uc5, uc6 :: UCChunk

uc1 = ucIO

uc2 = nw $ npnc "inputsource" (nounPhraseSP 
  "There will always be a source of input data external to the software.")

uc3 = ucOutput

uc4 = nw $ npnc "goal" (nounPhraseSP
  "The goal of the system is to calculate temperatures and energies.")

uc5 = nw $ npnc "odes" uc5np

uc6 = nw $ npnc "energy" (nounPhraseSP $
  "The energy equations can be defined using the parameters defined in the " ++
  "input parameters module.")
