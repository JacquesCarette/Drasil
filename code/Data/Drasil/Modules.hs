module Data.Drasil.Modules
  ( mod_hw
  , mod_behav
  , mod_sw
  , mod_seq
  , mod_linked
  , mod_assoc
  , mod_vector
  , mod_ctrl
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

mod_assoc_serv :: ConceptChunk
mod_assoc_serv = dccWDS "mod_assoc_serv" (cn' "associative data structure")
    (S "Provides operations on hash tables, such as building a hash table, " :+:
    S "accessing a specific entry, etc.")

mod_vector_serv :: ConceptChunk
mod_vector_serv = dccWDS "mod_vector_serv" (cn' "vector")
    (S "Provides vector operations such as addition, scalar and vector " :+:
    S "multiplication, dot and cross products, rotations, etc.")

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

-- Behaviour Hiding Module
mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding
    (S "The contents of the required behaviors.")
    Nothing

-- sfwr desc module
mod_sw :: ModuleChunk
mod_sw = makeUnimpModule modSfwrDecision
    (S "The design decision based on mathematical theorems, physical facts" :+:
    S ", or programming considerations. The secrets of this module are " :+:
    S "not described in the SRS.")
    Nothing

-- Control module
mod_ctrl :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_ctrl impl depnd = makeImpModule modControl
  (S "The algorithm for coordinating the running of the program.")
  impl
  []
  []
  depnd
  (Just mod_behav)

mod_seq :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_seq impl depnd = makeImpModule mod_seq_serv 
    (S "The data structure for a sequence data type.") 
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_linked :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_linked impl depnd = makeImpModule mod_linked_serv
    (S "The data structure for a linked data type.")
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_assoc :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_assoc impl depnd = makeImpModule mod_assoc_serv
    (S "The data structure for an associative data type.")
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_vector :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_vector impl depnd = makeImpModule mod_vector_serv
    (S "The data structure representing vectors.")
    impl
    []
    []
    depnd
    (Just mod_sw)

{--
      -> [VarChunk]        -- module fields, aka state variables
      -> [MethodChunk]     -- the methods offered by module
      -> [ModuleChunk]     -- what modules this one depends on [extract!]
      -> Maybe ModuleChunk -- Parent module, for documents [extract!]
--}