module Data.Drasil.Modules
  ( mod_hw
  , mod_behav
  , mod_sw
  , mod_seq
  , mod_linked
  ) where

import Language.Drasil
import Data.Drasil.Concepts.Software

{-- Concept Chunks for Modules  --}

mod_seq_serv :: ConceptChunk
mod_seq_serv = dccWDS "mod_seq_serv" (cn' "sequence data structure")
    (S "Provides array manipulation operations, such as building an array " :+:
    S ", accessing a specific entry, slicing an array, etc.")

mod_linked_serv :: ConceptChunk
mod_linked_serv = dccWDS "mod_linked_serv" (cn' "linked data structure")
    (S "Provides tree manipulation operations, such as building a tree, " :+:
    S "accessing a specific entry, etc.")

{-- Module Chunks --}

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


mod_seq ::NamedIdea a => a -> ModuleChunk
mod_seq exampleName = makeImpModule mod_seq_serv 
    (S "The data structure for a sequence data type.") 
    exampleName
    []
    []
    []
    (Just mod_sw)

mod_linked ::NamedIdea a => a -> ModuleChunk
mod_linked exampleName = makeImpModule mod_linked_serv
    (S "The data structure for a linked data type.")
    exampleName
    []
    []
    []
    (Just mod_sw)