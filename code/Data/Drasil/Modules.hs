module Data.Drasil.Modules
  ( mod_hw
  , mod_behav
  , mod_sw
  ) where

import Language.Drasil
import Data.Drasil.Concepts.Software

mod_hw :: ModuleChunk
mod_hw = makeImpModule hwHiding
    (S "The data structure and algorithm used to implement the virtual " :+:
    S "hardware.")
    os
    []
    []
    []
    Nothing

mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding
    (S "The contents of the required behaviors.")
    Nothing


mod_sw :: ModuleChunk
mod_sw = makeUnimpModule modSfwrDecision
    (S "The design decision based on mathematical theorems, physical facts" :+:
    S ", or programming considerations. The secrets of this module are " :+:
    S "not described in the SRS.")
    Nothing