module Drasil.GlassBR.Modules where

import Language.Drasil
import Data.Drasil.Concepts.Software
import Data.Drasil.Concepts.Math
import Drasil.GlassBR.Concepts

import Data.Drasil.Modules
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Computation


modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputc, mod_outputf,
   mod_derivedv, mod_calc, mod_ctrl, mod_interpd, mod_sw, mod_interp]

-- input format module
mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun glassBRProg [] [mod_hw, mod_inputp] 
  (plural inDatum) modInputFormat

mod_inputp :: ModuleChunk
mod_inputp = mod_io_fun glassBRProg [] [mod_inputc] 
  (phrase input_ +:+ (plural parameter)) modInputParam --FIXME: Plural?

mod_inputc :: ModuleChunk
mod_inputc = mod_inputc_fun glassBRProg

mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The" +:+ (phrase algorithm)) 
  glassBRProg [] 
  [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc, mod_interp, mod_outputf]

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (phrase input_ +:+ (plural parameter) 
  `sC` S "the demand, the capacity," +:+
  S "the probability of breakage, and both safety" +:+. plural requirement)

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun glassBRProg [] [mod_hw, mod_inputp] 
  (plural outDatum) mod_outputf_desc

-- derived values module
mod_derivedv :: ModuleChunk
mod_derivedv = mod_derivedv_fun glassBRProg [mod_inputp]

-- calculations module
glassBR_calcDesc :: Sentence
glassBR_calcDesc =(S "Defines the equations for solving for the probability of glass" +:+
   S "breakage, demand, and capacity using the" +:+ (plural parameter) +:+
   S "in the input" +:+ (plural parameter) +:+. S "module")

mod_calc :: ModuleChunk
mod_calc = mod_calc_fun (glassBR_calcDesc)
  (S "The" +:+ (plural equation) +:+ S "for predicting the probability of glass breakage," +:+
   S "capacity, and demand, using the" +:+ phrase input_ +:+. (plural parameter))
   glassBRProg
   []
   [mod_inputp]

-- interpolation data module
mod_interpd :: ModuleChunk
mod_interpd = mod_io_fun glassBRProg [] [] ((plural datum) +:+
  S "used for interpolation") modInterpDatum --FIXME: Plural?

mod_interp :: ModuleChunk
mod_interp = mod_interp_fun glassBRProg [mod_interpd]