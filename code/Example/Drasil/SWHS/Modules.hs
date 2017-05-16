module Drasil.SWHS.Modules where
import Prelude hiding (id)
import Language.Drasil
import Drasil.SWHS.Concepts
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math
import Data.Drasil.Modules as M

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputv, mod_outputf,
          mod_outputv, mod_temp, mod_ener, mod_sw, Drasil.SWHS.Modules.mod_seq,
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
mod_inputf = makeImpModule modInputFormat (S "The format and structure of " :+:
             S "the input data.") swhsProg [] [] [M.mod_hw, mod_inputp, Drasil.SWHS.Modules.mod_seq] 
             (Just M.mod_behav)

-- Input Parameters Module
mod_inputp :: ModuleChunk
mod_inputp = makeImpModule modInputParam (S "The format and structure of " :+: --FIXME: Plural?
             S "the input parameters.") swhsProg [] [] [Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Input Verification Module
mod_inputv :: ModuleChunk
mod_inputv = makeImpModule modInputVerif (S "The format and structure of " :+:
             S "the physical and software constraints.") swhsProg [] [] 
             [mod_inputp, Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Output Format Module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = dccWDS "mod_outputf_desc" (cn' "output format") (
  S "Outputs the results of the calculations, including the input parameters," +:+
  S "temperatures, energies, and times when melting starts and stops.")

mod_outputf :: ModuleChunk
mod_outputf = makeImpModule mod_outputf_desc (S "The format and structure " :+:
              S "of the output data.") swhsProg [] [] [M.mod_hw, mod_inputp, Drasil.SWHS.Modules.mod_seq] 
              (Just M.mod_behav)

-- Output Verification Module
mod_outputv_desc :: ConceptChunk
mod_outputv_desc = dccWDS "mod_outputv_desc" (cn' "output verification") (
  S "Verifies that the output " :+:
  S "energy results follow the law of conservation of " :+:
  S "energy. Throws a warning if the relative error " :+:
  S "exceeds the error threshold.")

mod_outputv :: ModuleChunk
mod_outputv = makeImpModule mod_outputv_desc (S "The algorithm used to " :+:
              S "approximate expected results.") swhsProg [] [] 
              [mod_inputp, Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Temperature ODEs Module
mod_temp_desc :: ConceptChunk
mod_temp_desc = dccWDS "mod_temp_desc" (nounPhraseSP "temperature ODEs") (
  S "Defines the" +:+ (short ode) :+: S "s using the parameters in the" +:+ --FIXME use a pural abbreviation?
  S "input parameters module.")

mod_temp :: ModuleChunk
mod_temp = makeImpModule mod_temp_desc (S "The " :+: (short ode) :+:
           S "s for solving the temperature, using the input parameters.")
           swhsProg [] [] [mod_inputp, Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Energy Equations Module
mod_ener_desc :: ConceptChunk
mod_ener_desc = dccWDS "mod_ener_desc" (nounPhraseSP "energy equations") (
  S "Defines the energy equations using the parameters in the input" +:+
  S "parameters module.")

mod_ener :: ModuleChunk
mod_ener = makeImpModule mod_ener_desc (S "The equations for solving for " :+:
           S "the energies using the input parameters.") swhsProg [] [] 
           [mod_inputp, Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Control Module
--mod_ctrl :: ModuleChunk
--mod_ctrl = makeImpModule modControl (S "The algorithm for coordinating " :+:
--           S "the running of the prograM.") swhsProg [] [] [M.mod_hw, mod_inputp, 
--           mod_inputf, mod_inputv, mod_temp, mod_ener, mod_ode, mod_plot, 
--           mod_outputv, mod_outputf, Drasil.SWHS.Modules.mod_seq] (Just M.mod_behav)

-- Software Decision Module
--mod_sw :: ModuleChunk
--mod_sw = makeUnimpModule modSfwrDecision (S "The design decision based on " :+:
--         S "mathematical theorems, physical facts, or programming " :+:
--         S "considerations. The secrets of this module are not described " :+:
--         S "in the " :+: (short srs) :+: S ".") Nothing

-- Sequence Data Structure Module
mod_seq_desc :: ConceptChunk
mod_seq_desc = dccWDS "mod_seq_desc" (nounPhraseSP "sequence data structure")
  (S "Provides array manipulation" :+:
  S ", including building an array, accessing a specific entry" :+:
  S ", slicing an array, etc.")

mod_seq :: ModuleChunk
mod_seq = M.mod_seq matlab []

-- ODE Solver Module
mod_ode_desc :: ConceptChunk
mod_ode_desc = dccWDS "mod_ode_desc" (nounPhraseSP "ODE solver") (
  S "Provides solvers that take the governing equation, initial conditions," +:+ 
  S "and numerical parameters, and solve them.")

mod_ode :: ModuleChunk
mod_ode = makeImpModule mod_ode_desc (S "The algorithm to solve a system of" :+:
          S " first order " :+: (short ode) :+: S "s.") matlab [] [] 
          [Drasil.SWHS.Modules.mod_seq] (Just M.mod_sw)

-- Plotting Module
mod_plot_desc :: ConceptChunk
mod_plot_desc = dcc "mod_plot_desc" (nounPhraseSP "plotting")
  "Provides a plot function."

mod_plot :: ModuleChunk
mod_plot = makeImpModule mod_plot_desc (S "The data structures and " :+:
           S "algorithms for plotting data graphically.") matlab [] [] [Drasil.SWHS.Modules.mod_seq] 
           (Just M.mod_sw)
