module Example.Drasil.Modules where

import Language.Drasil
import Control.Lens ((^.))
import Example.Drasil.HeatTransfer
import Example.Drasil.Concepts

-- Calc Modules
meth_h_g, meth_h_c :: MethodChunk
meth_h_g = fromEC h_g
meth_h_c = fromEC h_c

mod_calc_desc :: ConceptChunk
mod_calc_desc = CC "calc" (S "Calculates heat transfer coefficients")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations used to calculate heat transfer coefficients")
  program
  [meth_h_g, meth_h_c]


-- HW Hiding Module
mod_hw_desc :: ConceptChunk
mod_hw_desc = CC "hardware hiding"
  (S "Serves as a virtual hardware used by the rest of the system. This " :+:
   S "module provides the interface between the hardware and the software. " :+:
   S "So, the system can use it to display outputs or to accept inputs.")

mod_hw :: ModuleChunk
mod_hw = makeImpModule mod_hw_desc
  (S "The data structure and algorithm used to implement the virtual hardware.")
  os
  []


-- Behaviour Hiding Module
mod_behav_desc :: ConceptChunk
mod_behav_desc = CC "behaviour hiding"
  (S "Includes programs that provide externally visible behavior of " :+:
   S "the system as specified in the software requirements specification " :+:
   S "(SRS) documents. This module serves as a communication layer between " :+:
   S "the hardware-hiding module and the software decision module. The " :+:
   S "programs in this module will need to change if there are changes " :+:
   S "in the SRS.")

mod_behav :: ModuleChunk
mod_behav = makeUnimpModule mod_behav_desc
  (S "The contents of the required behaviors.")