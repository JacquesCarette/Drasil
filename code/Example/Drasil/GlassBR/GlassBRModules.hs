module Example.Drasil.GlassBR.GlassBRModules where

import Language.Drasil
import Example.Drasil.GlassBR.GlassBRConcepts

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputc, mod_outputf, 
  mod_derivedv, mod_calc, mod_ctrl, mod_interpd, mod_sw, mod_interp]

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

-- input format module
mod_inputf_desc :: ConceptChunk
mod_inputf_desc = CC "input format"
  (S "Converts the input data into the data structure used by the input " :+:
   S "parameters module.")

mod_inputf :: ModuleChunk
mod_inputf = makeImpModule mod_inputf_desc
  (S "The format and structure of the input data.")
  program
  []
  [mod_hw, mod_inputp]
  (Just mod_behav)

-- input parameters module
mod_inputp_desc :: ConceptChunk
mod_inputp_desc = CC "input parameters"
  (S "Stores the parameters needed for the program, including material " :+:
   S "properties, processing conditions and numerical parameters. The " :+:
   S "values can be read as needed. This module knows how many parameters " :+:
   S "it stores.")

mod_inputp :: ModuleChunk
mod_inputp = makeImpModule mod_inputp_desc
  (S "The format and structure of the input parameters.")
  program
  []
  [mod_inputc]
  (Just mod_behav)

-- input constraints module
mod_inputc_desc :: ConceptChunk
mod_inputc_desc = CC "input constraints"
  (S "Defines the constraints on the input data and gives an error if " :+:
   S "a constraint is violated.")

mod_inputc :: ModuleChunk
mod_inputc = makeImpModule mod_inputc_desc
  (S "The constraints on the input data.")
  program
  []
  []
  (Just mod_behav)

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = CC "output format"
  (S "Outputs the results of the calculations, including the input " :+:
   S "parameters, the demand, the capacity, the probability of breakage, " :+:
   S "and both safety requirements.")

mod_outputf :: ModuleChunk
mod_outputf = makeImpModule mod_outputf_desc
  (S "The format and structure of the output data.")
  program
  []
  [mod_hw, mod_inputp]
  (Just mod_behav)

-- derived values module
mod_derivedv_desc :: ConceptChunk
mod_derivedv_desc = CC "derived values"
  (S "Defines the equations transforming the initial inputs into derived " :+:
   S "quantities.")

mod_derivedv :: ModuleChunk
mod_derivedv = makeImpModule mod_derivedv_desc
  (S "The transformations from initial inputs to derived quantities.")
  program
  []
  [mod_inputp]
  (Just mod_behav)

-- calculations module
mod_calc_desc :: ConceptChunk
mod_calc_desc = CC "calculations"
  (S "Defines the equations for solving for the probability of glass " :+:
   S "breakage, demand, and capacity using the parameters in the input " :+:
   S "parameters module.")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations for predicting the probability of glass breakage, " :+:
   S "capacity, and demand, using the input parameters.")
   program
   []
   [mod_inputp]
   (Just mod_behav)

-- Control module
mod_ctrl_desc :: ConceptChunk
mod_ctrl_desc = CC "control" (S "Provides the main program.")

mod_ctrl :: ModuleChunk
mod_ctrl = makeImpModule mod_ctrl_desc
  (S "The algorithm for coordinating the running of the program.")
  program
  []
  [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc,
  mod_interp, mod_outputf]
  (Just mod_behav)

-- interpolation data module
mod_interpd_desc :: ConceptChunk
mod_interpd_desc = CC "interpolation data"
  (S "Converts the input interpolation data into the data structure used " :+:
   S "by the interpolation module.")

mod_interpd :: ModuleChunk
mod_interpd = makeImpModule mod_interpd_desc
  (S "The format and structure of the data used for interpolation.")
   program
   []
   []
   (Just mod_behav)

-- sfwr dec module
mod_sw_desc :: ConceptChunk
mod_sw_desc = CC "software decision"
  (S "Includes a data structure and algorithms used in the " :+:
   S "system that do not provide direct interaction with the user.")

mod_sw :: ModuleChunk
mod_sw = makeUnimpModule mod_sw_desc
  (S "The design decision based on mathematical theorems, " :+:
   S "physical facts, or programming considerations. The secrets of this " :+:
   S "module are not described in the SRS.")
   Nothing

-- interpolation module
mod_interp_desc :: ConceptChunk
mod_interp_desc = CC "interpolation"
  (S "Provides the equations that take the input parameters and " :+:
   S "interpolation data and return an interpolated value.")

mod_interp :: ModuleChunk
mod_interp = makeImpModule mod_interp_desc
  (S "The interpolation algorithm.")
   program
   []
   [mod_interpd]
   (Just mod_sw)
