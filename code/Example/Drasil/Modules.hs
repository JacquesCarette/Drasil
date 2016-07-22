module Example.Drasil.Modules where

import Language.Drasil
import Control.Lens ((^.))
import Example.Drasil.HeatTransfer
import Example.Drasil.Concepts

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
  Nothing


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
  Nothing


-- Calc Module
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
  (Just mod_behav)


-- fakes
mod_f1_d :: ConceptChunk
mod_f1_d = CC "fake1" (S "Does fake stuff")
mod_f1 :: ModuleChunk
mod_f1 = makeImpModule mod_f1_d (S "Fake things") program [] (Just mod_calc)

mod_f2_d :: ConceptChunk
mod_f2_d = CC "fake2" (S "Does fake stuff")
mod_f2 :: ModuleChunk
mod_f2 = makeImpModule mod_f2_d (S "Fake things") program [] (Just mod_calc)

mod_f3_d :: ConceptChunk
mod_f3_d = CC "fake3" (S "Does fake stuff")
mod_f3 :: ModuleChunk
mod_f3 = makeImpModule mod_f3_d (S "Fake things") program [] (Just mod_calc)