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
  (mod_ctrl_fun glassBRProg [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc, mod_interp, mod_outputf]),
  mod_interpd, mod_sw, mod_interp]

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

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = dccWDS "mod_outputf_desc" (cn' "output format")
  (S "Outputs the results of the calculations, including the input " :+:
   S "parameters, the demand, the capacity, the probability of breakage, " :+:
   S "and both safety requirements.")

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

-- TODO: Maybe this could be parameterized.
-- FIXME: Should use plural of "calculation"
mod_calc_desc :: ConceptChunk
mod_calc_desc = dccWDS "mod_calc_desc" (cn "calculations")
  (S "Defines the equations for solving for the probability of glass " :+:
   S "breakage, demand, and capacity using the parameters in the input " :+:
   S "parameters module.")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations for predicting the probability of glass breakage, " :+:
   S "capacity, and demand, using the input parameters.")
   glassBRProg
   []
   []
   [mod_inputp]
   (Just mod_behav)

-- interpolation data module

mod_interpd :: ModuleChunk
mod_interpd = mod_io_fun glassBRProg [] ((plural datum) +:+ S "used for interpolation") modInterpDatum --FIXME: Plural?

-- interpolation module

mod_interp :: ModuleChunk
mod_interp = makeImpModule modInterpolation
  (S "The interpolation algorithm.")
   glassBRProg
   []
   []
   [mod_interpd]
   (Just mod_sw)
