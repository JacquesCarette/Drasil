module Data.Drasil.Modules
  ( mod_hw,
    mod_behav,
    mod_sw,
    mod_seq_fun,
    mod_linked_fun,
    mod_assoc_fun,
    mod_vector_fun,
    mod_ctrl_fun,
    mod_io_fun,
    mod_param_fun,
    mod_plot_fun,
    mod_rng_fun,
    mod_outputf_desc_fun,
    mod_ode_desc,
    mod_ode_fun,
    mod_calc_fun,
    mod_interp_fun,
    mod_inputc_fun,
    mod_derivedv_fun
  ) where

import Language.Drasil
import Data.Drasil.Utils (foldlSent)
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Computation
import Control.Lens ((^.))
import Data.Drasil.Concepts.Math(ode)
import Data.Drasil.Concepts.Documentation

{-- Module Chunks --}

mod_hw :: ModuleChunk
mod_hw = makeImpModule hwHiding
  (foldlSent [S "The", (plural $ dataStruct), 
  S "and", (phrase $ algorithm ^. term), S "used to implement the virtual",
  S "hardware"])
  os
  []
  []
  []
  Nothing

-- Behaviour Hiding Module
mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding
  (S "The contents of the required behaviours.")
  Nothing

-- sfwr desc module
mod_sw :: ModuleChunk
mod_sw = makeUnimpModule modSfwrDecision
  (S "The design decision based on mathematical theorems, physical facts" `sC`
  S "or programming considerations. The secrets of this module are" +:+
  S "not described in the SRS.")
  Nothing

-- Control module
mod_ctrl_fun :: NamedIdea a => Sentence -> a -> [MethodChunk] -> [ModuleChunk] -> ModuleChunk
mod_ctrl_fun desc impl mthd depnd = makeImpModule modControl
  (foldlSent [desc, S "for coordinating the running of the program"])
  impl
  []
  mthd
  depnd
  (Just mod_behav)

mod_io_fun :: NamedIdea a => a -> [MethodChunk] -> [ModuleChunk] -> Sentence -> ConceptChunk -> ModuleChunk
mod_io_fun impl mthd depnd desc cChunk = makeImpModule cChunk
  (foldlSent [S "The format and", (phrase structure), S "of the", desc])
  impl
  []
  mthd
  depnd
  (Just mod_behav)

mod_param_fun :: NamedIdea a => a -> [ModuleChunk] -> Sentence -> ConceptChunk -> ModuleChunk
mod_param_fun impl depnd desc cChunk = makeImpModule cChunk
  (foldlSent [desc])
  impl
  []
  []
  depnd
  (Just mod_behav)

mod_seq_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_seq_fun impl depnd = makeImpModule mod_seq_serv 
  (foldlSent [S "The", (plural $ dataStruct), S "for a sequence", 
  (plural dataType)]) 
  impl
  []
  []
  depnd
  (Just mod_sw)

mod_linked_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_linked_fun impl depnd = makeImpModule mod_linked_serv
  (foldlSent [S "The", (plural $ dataStruct), S "for a linked", 
  (plural dataType)])
  impl
  []
  []
  depnd
  (Just mod_sw)

mod_assoc_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_assoc_fun impl depnd = makeImpModule mod_assoc_serv
  (foldlSent [S "The", (plural $ dataStruct), S "for an associative", 
  (plural dataType)])
  impl
  []
  []
  depnd
  (Just mod_sw)

mod_vector_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_vector_fun impl depnd = makeImpModule mod_vector_serv
  (foldlSent [S "The", (plural $ dataStruct), S "representing vectors"])
  impl
  []
  []
  depnd
  (Just mod_sw)

mod_plot_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_plot_fun impl depnd = makeImpModule mod_plot_desc
  (foldlSent [S "The", (plural $ dataStruct'), 
  S "and", (plural $ algorithm ^. term), S  "for plotting data graphically"])
  impl
  []
  []
  depnd
  (Just mod_sw)
   
mod_rng_fun :: NamedIdea a => a -> [ModuleChunk] -> Sentence -> ModuleChunk
mod_rng_fun impl depnd desc = makeImpModule (dccWDS "mod_rng_desc" (cn' "random number generator") desc)
  (foldlSent [S "Pseudo-random number generation", (phrase $ algorithm ^. term)])
  impl
  []
  []
  depnd
  (Just mod_sw)

mod_ode_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_ode_fun impl depnd = makeImpModule mod_ode_desc (S "The algorithm to solve a system of" :+:
  S " first order " :+: (short ode) :+: S "s.")
  impl
  [] 
  [] 
  depnd
  (Just mod_sw)

mod_calc_fun :: NamedIdea a => Sentence -> Sentence -> a -> [MethodChunk] -> [ModuleChunk] -> ModuleChunk
mod_calc_fun defn' desc impl depnd1 depnd2 = makeImpModule (mod_calc_desc defn')
  (desc)
  impl
  []
  depnd1
  depnd2
  (Just mod_behav)

-- interpolation module
mod_interp_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_interp_fun impl depnd = makeImpModule modInterpolation
  (S "The interpolation algorithm.")
   impl
   []
   []
   depnd
   (Just mod_sw)

-- input constraints module --should be its own module?
mod_inputc_fun :: NamedIdea a => a -> ModuleChunk
mod_inputc_fun impl= makeImpModule modInputConstraint --FIXME: Plural?
  (S "The" +:+ plural constraint +:+ S "on the" +:+ phrase input_ +:+. (plural datum))
  impl
  []
  []
  []
  (Just mod_behav)

-- derived values module
mod_derivedv_fun :: NamedIdea a => a -> [ModuleChunk] -> ModuleChunk
mod_derivedv_fun impl depnd= makeImpModule modDerivedVal --FIXME: Plural?
  (S "The transformations from initial" +:+ plural input_ +:+. S "to derived quantities")
  impl
  []
  []
  depnd
  (Just mod_behav)


{--
      -> [VarChunk]        -- module fields, aka state variables
      -> [MethodChunk]     -- the methods offered by module
      -> [ModuleChunk]     -- what modules this one depends on [extract!]
      -> Maybe ModuleChunk -- Parent module, for documents [extract!]
--}