module Data.Drasil.Modules
  ( mod_hw
  , mod_behav
  , mod_sw
  , mod_seq_fun
  , mod_linked_fun
  , mod_assoc_fun
  , mod_vector_fun
  , mod_ctrl_fun
  , mod_inputf_fun
  , mod_inputp_fun
  ) where

import Language.Drasil
import Data.Drasil.Concepts.Software

{-- Concept Chunks for Modules  --}

mod_seq_serv :: ConceptChunk
mod_seq_serv = dccWDS "mod_seq_serv" (cn' "sequence data structure")
    (S "Provides array manipulation operations, such as building an array" `sC`
    S "accessing a specific entry, slicing an array, etc.")

mod_linked_serv :: ConceptChunk
mod_linked_serv = dccWDS "mod_linked_serv" (cn' "linked data structure")
    (S "Provides tree manipulation operations, such as building a tree" `sC`
    S "accessing a specific entry, etc.")

mod_assoc_serv :: ConceptChunk
mod_assoc_serv = dccWDS "mod_assoc_serv" (cn' "associative data structure")
    (S "Provides operations on hash tables, such as building a hash table" `sC`
    S "accessing a specific entry, etc.")

mod_vector_serv :: ConceptChunk
mod_vector_serv = dccWDS "mod_vector_serv" (cn' "vector")
    (S "Provides vector operations such as addition, scalar and vector" +:+
    S "multiplication, dot and cross products, rotations, etc.")

{-- Module Chunks --}

mod_hw :: ModuleChunk
mod_hw = makeImpModule hwHiding
    (S "The data structure and algorithm used to implement the virtual" +:+
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
    (S "The design decision based on mathematical theorems, physical facts" `sC`
    S "or programming considerations. The secrets of this module are" +:+
    S "not described in the SRS.")
    Nothing

-- Control module
mod_ctrl_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_ctrl_fun impl depnd = makeImpModule modControl
  (S "The algorithm for coordinating the running of the program.")
  impl
  []
  []
  depnd
  (Just mod_behav)

-- parameterize inputf and inputp into one mod_input?
mod_inputf_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_inputf_fun impl depnd = makeImpModule modInputFormat
  (S "The format and structure of the input data.")
  impl
  []
  []
  depnd
  (Just mod_behav)

mod_inputp_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_inputp_fun impl depnd= makeImpModule modInputParam --FIXME: Plural?
  (S "The format and structure of the input parameters.")
  impl
  []
  []
  depnd
  (Just mod_behav)

mod_seq_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_seq_fun impl depnd = makeImpModule mod_seq_serv 
    (S "The data structure for a sequence data type.") 
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_linked_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_linked_fun impl depnd = makeImpModule mod_linked_serv
    (S "The data structure for a linked data type.")
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_assoc_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_assoc_fun impl depnd = makeImpModule mod_assoc_serv
    (S "The data structure for an associative data type.")
    impl
    []
    []
    depnd
    (Just mod_sw)

mod_vector_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_vector_fun impl depnd = makeImpModule mod_vector_serv
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