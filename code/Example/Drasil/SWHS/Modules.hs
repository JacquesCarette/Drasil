module Drasil.SWHS.Modules where
import Prelude hiding (id)
import Language.Drasil
import Drasil.SWHS.Concepts
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math hiding (constraint)
import Data.Drasil.Concepts.Computation
import Data.Drasil.Concepts.Documentation
import Control.Lens ((^.))
import Data.Drasil.Modules
import Data.Drasil.Software.Products
import Data.Drasil.Quantities.Thermodynamics (temp)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Concepts.Thermodynamics (law_cons_energy, melting)

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputv, mod_outputf,
          mod_outputv, mod_temp, mod_ener, mod_sw, mod_seq,
          mod_ode, mod_plot]

-- HW Hiding Module
--mod_hw :: ModuleChunk
--mod_hw = makeImpModule hwHiding (S "The data structure and algorithm " :+:
--         S "used to implement the virtual hardware.") os [] [] [] Nothing

-- Behaviour Hiding Module
--mod_behav :: ModuleChunk
--mod_behav = makeUnimpModule modBehavHiding (S "The contents of the required" :+:
--            S " behaviours.") Nothing

-- Input Format Module
mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun
  swhsProg [] [mod_hw, mod_inputp, mod_seq] (phrase input_ +:+ plural datum) modInputFormat

-- Input Parameters Module
mod_inputp :: ModuleChunk
mod_inputp = mod_io_fun
  swhsProg [] [mod_seq] (phrase input_ +:+ plural parameter) modInputParam

-- Input Verification Module
mod_inputv :: ModuleChunk
mod_inputv = mod_io_fun
  swhsProg [] [mod_inputp, mod_seq] (phrase physical +:+ S "and" +:+
  phrase software +:+ plural constraint) modInputVerif

-- Output Format Module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (phrase input_ +:+ plural parameter `sC`
  plural temp `sC` plural energy `sC` S "and" +:+ plural time +:+ S "when" +:+
  phrase melting +:+. S "starts and stops")

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun
  swhsProg [] [mod_hw, mod_inputp, mod_seq] (phrase output_ +:+ plural datum) mod_outputf_desc

-- Output Verification Module
mod_outputv_desc :: ConceptChunk
mod_outputv_desc = dccWDS "mod_outputv_desc" (cn' "output verification") (
  S "Verifies that the" +:+ phrase output_ +:+ phrase energy +:+
  S "results follow the" +:+. phrase law_cons_energy +:+
  S "Throws a warning if the relative error exceeds the error threshold.")

mod_outputv :: ModuleChunk
mod_outputv = mod_param_fun swhsProg [mod_inputp, mod_seq]
  (S "The algorithm used to approximate expected results") mod_outputv_desc

-- Temperature ODEs Module
mod_temp_desc :: ConceptChunk
mod_temp_desc = dccWDS "mod_temp_desc" (nounPhraseSP "temperature ODEs") (
  S "Defines the" +:+ (short ode) :+: S "s" --FIXME uses a plural abbreviation?
  +:+ S "using the" +:+ plural parameter +:+ S "in the" +:+
  phrase input_ +:+ plural parameter +:+. phrase module_)

mod_temp :: ModuleChunk
mod_temp = makeImpModule mod_temp_desc (S "The " :+: (short ode) :+: S "s" +:+
  S "for solving the" +:+ phrase temp `sC` S "using the" +:+ phrase input_ +:+.
  plural parameter) swhsProg [] [] [mod_inputp, mod_seq] (Just mod_behav)

-- Energy Equations Module
mod_ener_desc :: ConceptChunk
mod_ener_desc = dccWDS "mod_ener_desc" (nounPhraseSP "energy equations") (
  S "Defines the" +:+ phrase energy +:+ plural equation +:+ S "using the" +:+
  plural parameter +:+ S "in the" +:+ phrase input_ +:+ plural parameter +:+.
  phrase module_)

mod_ener :: ModuleChunk
mod_ener = mod_param_fun swhsProg [mod_inputp, mod_seq]
  (S "The" +:+ plural equation +:+ S "for solving for the" +:+ plural energy +:+
  S "using the" +:+ phrase input_ +:+ plural parameter) mod_ener_desc

-- Control Module
--mod_ctrl :: ModuleChunk
--mod_ctrl = makeImpModule modControl (S "The algorithm for coordinating " :+:
--           S "the running of the prograM.") swhsProg [] [] [M.mod_hw, mod_inputp,
--           mod_inputf, mod_inputv, mod_temp, mod_ener, mod_ode, mod_plot,
--           mod_outputv, mod_outputf, mod_seq] (Just M.mod_behav)
mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The" +:+ phrase algorithm)
  swhsProg [] [mod_hw, mod_inputp, mod_inputf, mod_inputv, mod_temp, mod_ener,
  mod_ode, mod_plot, mod_outputv, mod_outputf, mod_seq]

-- Software Decision Module
--mod_sw :: ModuleChunk
--mod_sw = makeUnimpModule modSfwrDecision (S "The design decision based on " :+:
--         S "mathematical theorems, physical facts, or programming " :+:
--         S "considerations. The secrets of this module are not described " :+:
--         S "in the " :+: (short srs) :+: S ".") Nothing

-- Sequence Data Structure Module
--mod_seq_desc :: ConceptChunk
--mod_seq_desc = dccWDS "mod_seq_desc" (nounPhraseSP "sequence data structure")
--  (S "Provides array manipulation" :+:
--  S ", including building an array, accessing a specific entry" :+:
--  S ", slicing an array, etc.")

mod_seq :: ModuleChunk
mod_seq = mod_seq_fun matlab []

-- ODE Solver Module
--mod_ode_desc :: ConceptChunk
--mod_ode_desc = dccWDS "mod_ode_desc" (nounPhraseSP "ODE solver") (
--  S "Provides solvers that take the governing equation, initial conditions," +:+
--  S "and numerical parameters, and solve them.")

--mod_ode :: ModuleChunk
--mod_ode = makeImpModule mod_ode_desc (S "The algorithm to solve a system of" :+:
--          S " first order " :+: (short ode) :+: S "s.") matlab [] []
--          [mod_seq] (Just mod_sw)

mod_ode :: ModuleChunk
mod_ode = mod_ode_fun matlab [mod_seq]

-- Plotting Module
--mod_plot_desc :: ConceptChunk
--mod_plot_desc = dcc "mod_plot_desc" (nounPhraseSP "plotting")
--  "Provides a plot function."

--mod_plot :: ModuleChunk
--mod_plot = makeImpModule mod_plot_desc (S "The data structures and " :+:
--           S "algorithms for plotting data graphically.") matlab [] [] [mod_seq]
--           (Just mod_sw)

mod_plot :: ModuleChunk
mod_plot = mod_plot_fun matlab [mod_seq]