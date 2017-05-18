module Drasil.GlassBR.Modules where

import Language.Drasil
import Data.Drasil.Concepts.Software
import Drasil.GlassBR.Concepts

import Data.Drasil.Modules
import Data.Drasil.Concepts.Documentation

-- Some of the content below is 'generic' and should be pulled out from here.
-- And the constructors for making 'modules' should be rethought to be more
-- convenient for the most common cases.
modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputc, mod_outputf, mod_derivedv, mod_calc,
  mod_ctrl, mod_interpd, mod_sw, mod_interp]

-- input format module
mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun glassBRProg [mod_hw, mod_inputp] (S "input" +:+ (plural datum)) modInputFormat

mod_inputp :: ModuleChunk
mod_inputp = mod_io_fun glassBRProg [mod_inputc] (S "input parameters") modInputParam --FIXME: Plural?

-- input constraints module
mod_inputc :: ModuleChunk
mod_inputc = makeImpModule modInputConstraint --FIXME: Plural?
  (S "The constraints on the input data.")
  glassBRProg
  []
  []
  []
  (Just mod_behav)

mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun glassBRProg [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc, mod_interp, mod_outputf]

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (S "input parameters, the demand, the capacity, " +:+
  S "the probability of breakage, and both safety requirements.")

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun glassBRProg [mod_hw, mod_inputp] (S "output" +:+ (plural datum)) mod_outputf_desc

-- derived values module
mod_derivedv :: ModuleChunk
mod_derivedv = makeImpModule modDerivedVal --FIXME: Plural?
  (S "The transformations from initial inputs to derived quantities.")
  glassBRProg
  []
  []
  [mod_inputp]
  (Just mod_behav)

-- calculations module

glassBR_calcDesc :: Sentence
glassBR_calcDesc =(S "Defines the equations for solving for the probability of glass " :+:
   S "breakage, demand, and capacity using the parameters in the input " :+:
   S "parameters module.")

mod_calc :: ModuleChunk
mod_calc = mod_calc_fun (glassBR_calcDesc)
  (S "The equations for predicting the probability of glass breakage, " :+:
   S "capacity, and demand, using the input parameters.")
   glassBRProg
   []
   [mod_inputp]

-- interpolation data module

mod_interpd :: ModuleChunk
mod_interpd = mod_io_fun glassBRProg [] ((plural datum) +:+ S "used for interpolation") modInterpDatum --FIXME: Plural?

mod_interp :: ModuleChunk
mod_interp = mod_interp_fun glassBRProg [mod_interpd]