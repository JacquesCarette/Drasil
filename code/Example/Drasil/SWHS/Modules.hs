module Drasil.SWHS.Modules where

import Language.Drasil
import Drasil.SWHS.Concepts
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Software

import Control.Lens ((^.))

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputv, mod_outputf,
          mod_outputv, mod_temp, mod_ener, mod_ctrl, mod_sw, mod_seq, mod_ode,
          mod_plot]

-- HW Hiding Module
mod_hw :: ModuleChunk
mod_hw = makeImpModule modHWHiding (S "The data structure and algorithm " :+:
         S "used to implement the virtual hardware.") os [] [] [] Nothing

-- Behaviour Hiding Module
mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding (S "The contents of the required" :+:
            S " behaviours.") Nothing

-- Input Format Module
mod_inputf :: ModuleChunk
mod_inputf = makeImpModule modInputFormat (S "The format and structure of " :+:
             S "the input data.") swhsProg [] [] [mod_hw, mod_inputp, mod_seq] 
             (Just mod_behav)

-- Input Parameters Module
mod_inputp :: ModuleChunk
mod_inputp = makeImpModule modInputParams (S "The format and structure of " :+:
             S "the input parameters.") swhsProg [] [] [mod_seq] (Just mod_behav)

-- Input Verification Module
mod_inputv :: ModuleChunk
mod_inputv = makeImpModule modInputVerif (S "The format and structure of " :+:
             S "the physical and software constraints.") swhsProg [] [] 
             [mod_inputp, mod_seq] (Just mod_behav)

-- Output Format Module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = CC "output format" (S "Outputs the results of the " :+:
                   S "calculations, including the input parameters, " :+:
                   S "temperatures, energies, and times when melting starts" :+:
                   S " and stops.")

mod_outputf :: ModuleChunk
mod_outputf = makeImpModule mod_outputf_desc (S "The format and structure " :+:
              S "of the output data.") swhsProg [] [] [mod_hw, mod_inputp, mod_seq] 
              (Just mod_behav)

-- Output Verification Module
mod_outputv_desc :: ConceptChunk
mod_outputv_desc = CC "output verification" (S "Verifies that the output " :+:
                   S "energy results follow the law of conservation of " :+:
                   S "energy. Throws a warning if the relative error " :+:
                   S "exceeds the error threshold.")

mod_outputv :: ModuleChunk
mod_outputv = makeImpModule mod_outputv_desc (S "The algorithm used to " :+:
              S "approximate expected results.") swhsProg [] [] 
              [mod_inputp, mod_seq] (Just mod_behav)

-- Temperature ODEs Module
mod_temp_desc :: ConceptChunk
mod_temp_desc = CC "temperature ODEs" (S "Defines the " :+: 
                S (ordDiffEq ^. name) :+: S "s using the parameters in the " :+:
                S "input parameters module.")

mod_temp :: ModuleChunk
mod_temp = makeImpModule mod_temp_desc (S "The " :+: S (ordDiffEq ^. name) :+:
           S "s for solving the temperature, using the input parameters.")
           swhsProg [] [] [mod_inputp, mod_seq] (Just mod_behav)

-- Energy Equations Module
mod_ener_desc :: ConceptChunk
mod_ener_desc = CC "energy equations" (S "Defines the energy equations " :+:
                S "using the parameters in the input parameters module.")

mod_ener :: ModuleChunk
mod_ener = makeImpModule mod_ener_desc (S "The equations for solving for " :+:
           S "the energies using the input parameters.") swhsProg [] [] 
           [mod_inputp, mod_seq] (Just mod_behav)

-- Control Module
mod_ctrl :: ModuleChunk
mod_ctrl = makeImpModule modControl (S "The algorithm for coordinating " :+:
           S "the running of the program.") swhsProg [] [] [mod_hw, mod_inputp, 
           mod_inputf, mod_inputv, mod_temp, mod_ener, mod_ode, mod_plot, 
           mod_outputv, mod_outputf, mod_seq] (Just mod_behav)

-- Software Decision Module
mod_sw :: ModuleChunk
mod_sw = makeUnimpModule modSfwrDecision (S "The design decision based on " :+:
         S "mathematical theorems, physical facts, or programming " :+:
         S "considerations. The secrets of this module are not described " :+:
         S "in the " :+: S (srs ^. name) :+: S ".") Nothing

-- Sequence Data Structure Module
mod_seq_desc :: ConceptChunk
mod_seq_desc = CC "sequence data structure" (S "Provides array manipulation" :+:
               S ", including building an array, accessing a specific entry" :+:
               S ", slicing an array, etc.")

mod_seq :: ModuleChunk
mod_seq = makeImpModule mod_seq_desc (S "The data structure for a sequence " :+:
          S "data type.") matlab [] [] [] (Just mod_sw)

-- ODE Solver Module
mod_ode_desc :: ConceptChunk
mod_ode_desc = CC "ODE solver" (S "Provides solvers that take the governing " :+:
          S "equation, initial conditions, and numerical parameters, and " :+:
          S "solve them.")

mod_ode :: ModuleChunk
mod_ode = makeImpModule mod_ode_desc (S "The algorithm to solve a system of" :+:
          S " first order " :+: S (ordDiffEq ^. name) :+: S "s.") matlab [] [] 
          [mod_seq] (Just mod_sw)

-- Plotting Module
mod_plot_desc :: ConceptChunk
mod_plot_desc = CC "plotting" (S "Provides a plot function.")

mod_plot :: ModuleChunk
mod_plot = makeImpModule mod_plot_desc (S "The data structures and " :+:
           S "algorithms for plotting data graphically.") matlab [] [] [mod_seq] 
           (Just mod_sw)